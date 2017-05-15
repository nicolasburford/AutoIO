;Function to display version of tools.
(defun c:AutoIO  ()
  (princ "\nAuto IO\n")
  (princ "Version 5.10")
  (princ))

;Additional function to run commands necessary for new updates
(defun setupautoio ()
  (if (not (FINDFILE "Test Form For Hex.dll"))
    (progn
      (LM:Copy
       "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Source Code\\VB\\Test Form For Hex.dll"
       "C:\\Drafting\\Custom\\AutoIO\\VB")))

  (c:AutoIO)
  (princ))

;Main update function called form AutoCAD
(defun c:autoioupdate ()
  (vl-load-com)

  (command "shell" "wscript \"C:\\Drafting\\Custom\\AutoIO\\Lisp\\update.vbs\"")

)
;//Test moving these to the cloud

; (defun c:AUTOIOPULL ()
;   (vl-load-com)
;
;   (command "shell" "wscript \"C:\\Drafting\\Custom\\AutoIO\\Update\\pull.vbs\"")
;
; )

(defun c:AutoIOLoad()
  ;Update all search paths
  (updatepaths)

  ;loading custom toolbar
  (if (=(menugroup "AutoIOTools") "AutoIOTools")
    (progn
      (command "cuiunload" "AutoIOTools")
      (command "._cuiload" "AutoIOTools"))

    (command "._cuiload" "AutoIOTools"))
    (c:AutoIO)
  (princ)
)

;Open help file
(defun c:autoiohelp ()
  (startapp
    "explorer.exe"
    "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\AutoIO Help.docx"))

;Converts a string with delimiter to a list
(defun LM:str->lst (str del / pos)
  (if (setq pos (vl-string-search del str))
    (cons (substr str 1 pos)
     (LM:str->lst (substr str (+ pos 1 (strlen del))) del))

    (list str)))



;Deletes all paths including autoio and adds all necessary paths
(defun updatepaths ()
  (setq  *files*
   (vla-get-files (vla-get-preferences (vlax-get-acad-object))))


  (setq paths (vla-get-SupportPath *files*))
  (setq paths (LM:str->lst paths ";"))
  (setq npaths "")
  (foreach path  paths
    (if  (not (vl-string-search "autoio" (strcase path T)))
      (setq npaths (strcat npaths path ";"))))



  (setq addpaths (strcat
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\Stamps;"
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\1734 Modules;"
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\1756 Modules;"
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\1769 Modules;"
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\1794 Modules;"
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\Network Blocks;"
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\Symbols;"
                  "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Templates;"
                  "C:\\Drafting\\Custom\\AutoIO\\Lisp;"
                  "C:\\Drafting\\Custom\\AutoIO\\VB;"))

;local lisp

 (setq newpaths (strcat npaths addpaths))
 (vla-put-SupportPath *files* newpaths)

; end use of *files*
 (vlax-release-object *files*)
 exit quitely
 (princ "Search Paths Updates Successfully.\n"))
