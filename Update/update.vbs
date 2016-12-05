

dim oFSO
set oFSO = CreateObject("Scripting.FileSystemObject")

MsgBox "Please quit AutoCAD. Once terminated, press OK.", 16, "AutoIO Update"
' wscript.echo "notepad not running"
' set acad = CreateObject("AutoCad.application")
' oFSO.CopyFolder "\\usnr.com\data\Eng\Electrical\New Electrical Design\Electrical\AutoIO Tools\Source Code", "C:\Drafting\Custom\AutoIO"
' acad.ActiveDocument.SendCommand "(update2)" & vbCr
MsgBox "AutoIO is now up to date."
'(command "shell" "wscript \"C:\Drafting\Custom\AutoIO\Update\update.vbs\"")
