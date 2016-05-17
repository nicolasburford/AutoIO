from pyautocad import Autocad, APoint


acad = Autocad()
acad.prompt("Hello, Autocad from Python\n")
print acad.doc.Name
