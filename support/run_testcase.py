import GPS
import os
from os.path import *
import libadalang as lal
import gs_utils

PROJECT_TEMPLATE = """with "%(parent_path)s";
project %(project_name)s is
   for Source_Dirs use (".");
   for Source_Files use ("%(main_file)s");
   for Main use ("%(main_file)s");
   for Object_Dir use ".obj/" & project'Name;
   for Exec_Dir use ".";
   package Builder renames %(parent)s.Builder;
   package Compiler renames %(parent)s.Compiler;
   package Binder renames %(parent)s.Binder;
   package Linker renames %(parent)s.Linker;
end  %(project_name)s;"""

MAIN_TEMPLATE = """with AUnit.Test_Cases.Simple_Main_Generic;
procedure %(main_name)s is new AUnit.Test_Cases.Simple_Main_Generic (%(test_case)s);
"""



class Console_Process(GPS.Console, GPS.Process):
    def on_output2(self, dummy, matched, unmatched):
        self.on_output(matched, unmatched)

    def on_output(self, matched, unmatched):
        self.write(unmatched + matched)

    def on_exit2(self, dummy, status, unmatched_output):
        self.on_exit(status, unmatched_output)

    def on_exit(self, status, unmatched_output):
        self.write(unmatched_output)
        if self.state:
            self.state = False
            GPS.Process(self.main + " --text", ".+",
            on_exit=self.on_exit2,
            on_match=self.on_output2)

    def on_destroy(self):
        self.kill()  # Will call on_exit

    def __init__(self, command, main, name="Unit Test"):
        self.state = True
        self.main = main
        GPS.Console.__init__(
            self, name,
            on_destroy=Console_Process.on_destroy,
            force=False)
        self.clear()
        GPS.Process.__init__(
            self, command, ".+",
            on_exit=Console_Process.on_exit,
            on_match=Console_Process.on_output)


def ada2file(name):
    return name.lower().replace(".", "-") + ".ads"


def proj2file(name):
    return name.lower().replace(".", "-") + ".gpr"


tc_name = None
main_name = None
test_case = None
lastFile = None


def runTest(f: GPS.File):
    global tc_name
    global main_name
    global test_case
    global lastFile

    context = lal.AnalysisContext()
    unit = context.get_from_file(f.name())
    if not lastFile:
        lastFile = f
    if unit.root:
        for node in unit.root.finditer(lambda n: n.is_a(lal.PackageDecl)):
            tc_name = (node.children[0].text)
            for type_node in node.finditer(lambda n: n.is_a(lal.ConcreteTypeDecl)):
                test_case = type_node.children[0].text
                for subtype_node in type_node.finditer(lambda n: n.is_a(lal.SubtypeIndication)):
                    if subtype_node.text.lower() == "AUnit.Test_Cases.Test_Case".lower():
                        main_name = "%s.Main" % tc_name
                        lastFile = f

    if main_name:
        f = lastFile
        targetDir = join(f.project().get_attribute_as_string("Project_Dir"),
                         f.project().get_attribute_as_string("Object_Dir"),
                         "auto-unit")

        parent = f.project().name()
        project_name = "%s.%s" % (parent, main_name.replace(".", "_"))

        params = {"parent_path": f.project().file().name(),
                  "parent": parent,
                  "test_case": test_case,
                  "project_name": project_name,
                  "main_file":  ada2file(main_name),
                  "main_name": main_name}

        if not exists(targetDir):
            os.makedirs(targetDir)
        project_path = join(targetDir, proj2file(project_name))
        with open(project_path, "w") as outf:
            outf.write(PROJECT_TEMPLATE % params)

        with open(join(targetDir, ada2file(main_name)), "w") as outf:
            outf.write(MAIN_TEMPLATE % params)

        Build = Console_Process(["gprbuild", "-P", "%s" % project_path],
                                join(targetDir, splitext(ada2file(main_name))[0]),
                                "Unit Test:%s" % main_name)
    else:
        GPS.MDI.dialog("No Test_Case for %s found" % basename(f.name()))


@gs_utils.interactive("Editor", toolbar="main", name="Run TC")
def on_run_test_clicked():
    runTest(GPS.current_context().file())
