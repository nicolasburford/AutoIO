import sys
import json
import re
from pyautocad import Autocad, APoint

if sys.argv[1] == 's':
    cmd = str(' '.join(sys.argv[2:])).replace('\\n','').replace("\"","").replace('\\r','').replace("\\","\"")
    cmd = re.sub(r'\) *\(', ')(',cmd)
    cmd = cmd.lstrip(' ').rstrip(' ')
    cmd += '\n'
if sys.argv[1] =='f':
    cmd = "(load \"" + sys.argv[2].replace('\\','\\\\') + "\")\n"
print cmd
acad = Autocad(create_if_not_exists=True)
acad.doc.SendCommand(cmd)
