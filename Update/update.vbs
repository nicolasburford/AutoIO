

dim oShell
dim oFSO
dim acad

set oShell = createobject("WScript.Shell")
set oFSO = CreateObject("Scripting.FileSystemObject")
set acad = CreateObject("AutoCad.application")

' oShell.Run "taskkill /im acad.exe", , True
' oFSO.CreateFolder "C:\Drafting\Custom\AutoIO\Test"
acad.ActiveDocument.SendCommand "AutoIO" & vbCr

C:\Drafting\Custom\AutoIO\Update\update.vbs

'(command "shell" "wscript \"C:\\Drafting\\Custom\\AutoIO\\Update\\update.vbs\"")
