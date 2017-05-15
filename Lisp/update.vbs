

dim oFSO
set oFSO = CreateObject("Scripting.FileSystemObject")

Set Http = CreateObject("MSXML2.ServerXMLHTTP.6.0")
dim bStrm: Set bStrm = createobject("Adodb.Stream")


Sub GetLatest()
  Http.Open "GET","https://github.com/nicolasburford/AutoIO/archive/master.zip",false
  Http.Send

  with bStrm
      .type = 1 '//binary
      .open
      .write Http.responseBody
      .savetofile "c:\Drafting\Custom\AutoIO\download.zip", 2 '//overwrite
  end with

  ExtractTo = "c:\Drafting\Custom\AutoIODL"
  Set fso = CreateObject("Scripting.FileSystemObject")
  If NOT fso.FolderExists(ExtractTo) Then
     fso.CreateFolder(ExtractTo)
  else
    fso.DeleteFolder(ExtractTo)
    fso.CreateFolder(ExtractTo)
  End If

  set objShell = CreateObject("Shell.Application")
  set FilesInZip=objShell.NameSpace("c:\Drafting\Custom\AutoIO\download.zip").items
  objShell.NameSpace(ExtractTo).CopyHere(FilesInZip)

  ' fso.DeleteFolder "c:\Drafting\Custom\AutoIO"
  ' fso.CreateFolder("c:\Drafting\Custom\AutoIO")
  fso.CopyFolder "C:\Drafting\Custom\AutoIODL\AutoIO-master", "C:\Drafting\Custom\AutoIO"
  '
  ' fso.DeleteFile "c:\Drafting\Custom\AutoIO\download.zip"
  fso.DeleteFolder "c:\Drafting\Custom\AutoIODL"
end Sub

' oFSO.CopyFolder "C:\Drafting\Custom\AutoIO", "C:\Users\nicolasburford\Desktop\Test"
' If MsgBox "Please quit AutoCAD. Once terminated, press OK.", 16, "AutoIO Update"
Function GetProcess(FindProc)
   Dim i
   Dim strComputer

   strComputer = "."

   Set objWMIService = GetObject("winmgmts:" _
       & "{impersonationLevel=impersonate}!\\" & strComputer & "\root\cimv2")
   Set colProcessList = objWMIService.ExecQuery _
       ("Select Name from Win32_Process WHERE Name LIKE '" & FindProc & "%'")

   If colProcessList.count>0 then
       GetProcess = true
   else
       GetProcess = false
   End if

End Function

Sub Update()
  If MsgBox("Please quit all instances of AutoCAD. Once terminated, press OK and wait for confirmation (This will take a moment).", vbOKCancel+vbSystemModal+vbExclamation, "AutoIO Update") = vbOK Then
    While GetProcess( "acad.exe" )
      If MsgBox("AutoCAD still running. Please make sure to close all instances. Once terminated, press OK and wait for confirmation (This will take a moment).", vbOKCancel+vbSystemModal+vbExclamation, "AutoIO Update") <> vbOK then
        Exit Sub
      end if
    Wend
    GetLatest
    ' oFSO.CopyFolder "\\usnr.com\data\Eng\Electrical\New Electrical Design\Electrical\AutoIO Tools\Source Code", "C:\Drafting\Custom\AutoIO"
    MsgBox("Update Complete.")
  End If
End Sub

Update

'
' ' wscript.echo "notepad not running"
' ' set acad = CreateObject("AutoCad.application")
' '
' ' acad.ActiveDocument.SendCommand "(update2)" & vbCr
' MsgBox "AutoIO is now up to date."
'(command "shell" "wscript \"C:\Drafting\Custom\AutoIO\Update\update.vbs\"")
