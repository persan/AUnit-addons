import GPS
import re

def parser(node_name, attrs, value):
    attr = dict()
    for a in re.findall('''(\w+)=['"](.*?)['"]\B''', attrs):
        attr[a[0]] = a[1]

    if node_name == "project":
        return [attr["name"]]

    elif node_name == "file":
        return [value]


def on_click(node_name, attrs, value):
    if node_name == "file":
        GPS.EditorBuffer.get(GPS.File(value))


xml = '''<project name='foo'>
     <file>source.adb</file>
     <file>source.xx</file>
     <file>source.2</file>
  </project>'''


view = GPS.XMLViewer("Dummy", 1, parser, on_click)
view.parse_string(xml)
