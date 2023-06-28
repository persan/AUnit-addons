import GPS
import os
from os.path import *
import libadalang as lal
import gs_utils
from enum import Enum, auto

PROJECT_TEMPLATE = """project %(project_name)s extends "%(parent_path)s" is

   for Source_Dirs use (".");

   for Source_Files use ("%(main_file)s");

   for Main use ("%(main_file)s");

   for Object_Dir use ".obj/" & project'Name;

   for Exec_Dir use ".";

end  %(project_name)s;
"""

MAIN_TESTCASE_TEMPLATE = """with AUnit.Test_Cases.Simple_Main_Generic;
procedure %(main_name)s is new AUnit.Test_Cases.Simple_Main_Generic (%(test_case)s);
"""

MAIN_TESTSUIT_TEMPLATE = """with AUnit.Run.Generic_Runner;
procedure %(main_name)s is new AUnit.Run.Generic_Runner (%(test_suit)s);
"""


class Console_Process(GPS.Console, GPS.Process):
    def on_output2(self, matched, unmatched):
        self.on_output(matched, unmatched)

    def on_output(self, matched, unmatched):
        self.write(unmatched + matched)

    def on_exit2(self, status, unmatched_output):
        self.on_exit(status, unmatched_output)

    def on_exit(self, status, unmatched_output):
        self.write(unmatched_output)
        if self.state:
            self.state = False
            GPS.Process(self.main + " --text", ".+",
                        on_exit=Console_Process.on_exit2,
                        on_match=Console_Process.on_output2)

    def on_destroy(self):
        #  self.kill()  # Will call on_exit
        pass

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
    return name.lower().replace(".", "-")


def proj2file(name):
    return name.lower().replace(".", "-") + ".gpr"


class TestKind(Enum):
    TEST_CASE = auto()
    TEST_SUIT = auto()
    TEST_RUNNER = auto()


class Analyzer:
    def __init__(self):
        self.tc_name:   str = None
        self.main_name: str = None
        self.test_case: str = None
        self.test_suit: str = None
        self.lastFile:  str = None
        self.test_kind: TestKind = None

    def analyze(self, f: GPS.File):
        unit = context.get_from_file(f.name())
        if not self.lastFile:
            self.lastFile = f
        if unit.root:
            for node in unit.root.finditer(lambda n: n.is_a(lal.PackageDecl)):
                self.tc_name = (node.children[0].text)
                for type_node in node.finditer(lambda n: n.is_a(lal.ConcreteTypeDecl)):
                    self.test_case = type_node.children[0].text
                    for subtype_node in type_node.finditer(lambda n: n.is_a(lal.SubtypeIndication)):
                        if subtype_node.text.lower() == "AUnit.Test_Cases.Test_Case".lower():
                            self.main_name = "%s.Main" % tc_name
                            self.lastFile = f
                            self.test_kind = TestKind.TEST_CASE
                            self.test_suit = None
                for type_node in node.finditer(lambda n: n.is_a(lal.SubpSpec)):
                    print("type_node.children: %s" % str(type_node.children))
                    self.test_suit = (type_node.children[1].text)
                    for ret_type_node in type_node.finditer(lambda n: n.is_a(lal.SubtypeIndication)):
                        print("ret_type_node.children: %s" % str(ret_type_node.children))

                        if ret_type_node.children[1].text.lower() == "AUnit.Test_Suites.Access_Test_Suite".lower():
                            self.main_name = "%s.Main" % test_suit
                            self.lastFile = f
                            self.test_kind = TestKind.TEST_SUIT
                            self.test_case = None
            if main_name:
                self.parent = f.project().name()
                self.project_name = "%s.%s" % (parent, main_name.replace(".", "_"))

                self.targetDir = \
                    join(f.project().get_attribute_as_string("Project_Dir"),
                         f.project().get_attribute_as_string("Object_Dir"),
                         "auto-AUnit")

                self.dictonary = {"tc_name": self.tc_name,
                                  "main_name": self.main_name,
                                  "test_suit": self.test_suit,
                                  "parent": self.parent,
                                  "project_name": self.project_name,
                                  "targetDir": self.targetDir}

        def getDict(self):
            return self.dictonary


analyzer = Analyzer()


def runTest(f: GPS.File):
    params = analyzer.analyze(f)

    if params:
        f = lastFile

        if not exists(analyzer.targetDir):
            os.makedirs(analyzer.targetDir)
        project_path = join(analyzer.targetDir, proj2file(analyzer.project_name))

        with open(project_path, "w") as outf:
            outf.write(PROJECT_TEMPLATE % params)

        if test_kind == TestKind.TEST_CASE:
            with open(join(targetDir, ada2file(analyzer.main_name)), "w") as outf:
                outf.write(MAIN_TESTCASE_TEMPLATE % params)
        elif test_kind == TestKind.TEST_SUIT:
            with open(join(targetDir, ada2file(analyzer.main_name)), "w") as outf:
                outf.write(MAIN_TESTSUIT_TEMPLATE % params)

        Build = Console_Process(["gprbuild", "-P", "%s" % analyzer.project_path],
                                join(targetDir,
                                     splitext(ada2file(analyzer.main_name))[0]),
                                "Unit Test:%s" % analyzer.main_name)
    else:
        GPS.MDI.dialog("No Test_Case for %s found" % basename(f.name()))


# Delete old
try:
    a = GPS.Action("Run TC")
    a.destroy_ui()
    a.unregister()
except:
    pass


@gs_utils.interactive("Editor", toolbar="main", name="Run TC")
def on_run_test_clicked():
    runTest(GPS.current_context().file())
