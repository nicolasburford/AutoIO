

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

  ExtractTo = "c:\Drafting\Custom\AutoIO\download"
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

  fso.CopyFolder "C:\Drafting\Custom\AutoIO\download\AutoIO-master", "\\usnr.com\data\Eng\Electrical\New Electrical Design\Electrical\AutoIO Tools\Source Code"

  fso.DeleteFile "c:\Drafting\Custom\AutoIO\download.zip"
  fso.DeleteFolder ExtractTo
end Sub

GetLatest
