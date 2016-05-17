;main dialog box code
(defun TEST_DCL2 ( / dcl_id)

  (setq dcl_id (load_dialog "dialogboxes.dcl"))    ;load main dialog
  (if (not (new_dialog "test_dcl2" dcl_id))
   (exit))
     ;if

 (setq cnames '("ControlLogix" "Flex" "Point" "Compact" "PLC5" "Network Switch" "Interconnect"));dropdown list

;initial variable conditions
 (if (setq ss (ssget "X" (list (cons 410 "09")(cons 2 "drawinginfo"))))
   (progn
     (setq blockRef (vlax-ename->vla-object (ssname ss 0)))
     (setq proj (getattributevalue blockRef "PROJNUM"))
     (setq name2 (getattributevalue blockRef "NUM2"))
     (setq conso (getattributevalue blockRef "CONSO"))
     (setq sizs (getattributevalue blockRef "SIZ"))
     (setq file (getattributevalue blockRef "FILE")))

   (progn
     (if (member "09" (layoutlist))
      (setq blockRef (insblock "09" '(30 -2 0) "drawinginfo.dwg")))

     (setq proj "YYYY")
     (setq name2 "055")
     (setq conso "E1M1")
     (setq sizs "0")
     (setq file "")))


 (setq userclick T)
;initial field conditions
 (set_tile "proj" proj)
 (set_tile "name2" name2)
 (set_tile "conso" conso)
 (set_tile "file" file)

;;;(set_tile "proj" "YYYY")
;;;(set_tile "name2" "XXX")
;;;(set_tile "conso" "E1M1")
;;;(set_tile "file" "C:\\Users\\nicolas.burford@usnr.com\\Documents\\Important CAD Files\\Personal\\6017 West Fraser New Boston IO List Rev 3.xls")
;(set_tile "file" "C:\\Users\\nicolas.burford@usnr.com\\Documents\\Important CAD Files\\Personal\\ControlLogix_Standard_IO.xls")
 (mode_tile "name" 2)

 (start_list "selections")        ;start the list box
 (mapcar 'add_list cnames)        ;fill the list box
 (end_list)
 (set_tile "selections" sizs)

;conditions when activated
 (action_tile "proj" "(setq proj $value)")
 (action_tile "name2" "(setq name2 $value)")
 (action_tile "conso" "(setq conso $value)")
 (action_tile "file" "(setq file $value)")
 (action_tile "browse" "(val2)")
 (action_tile
     "cancel"            ;if cancel button pressed
     "(done_dialog) (setq userclick nil)")    ;close dialog, set flag
    ;action_tile

 (action_tile
   "accept"            ;if O.K. pressed
   (strcat            ;string 'em together
     "(progn "
      "(setq rackdrop (atoi(get_tile \"rackdrop\"))) "
      "(setq SIZ (atof (get_tile \"selections\")))"  ;get list selection
     " (done_dialog) (setq userclick T))"))    ;close dialog, set flag
    ;strcat
  ;action tile

 (start_dialog)
 (unload_dialog dcl_id)

  ;save information to sheet 09
 (if userclick
   (progn
     (if (member "09" (layoutlist))
       (progn
        (setattributevalue blockRef "PROJNUM" proj)
        (setattributevalue blockRef "NUM2" name2)
        (setattributevalue blockRef "CONSO" conso)
        (setattributevalue blockRef "SIZ" siz)
        (setattributevalue blockRef "FILE" file)))


     (alert (strcat "Console: " proj "-" name2 "-" conso)))

  (alert "AutoIO cancelled."))


 (princ))

;defun
;dialog box


-------------------

-------------------

;browse button calls up the getfiled function to select a file
(defun val2 (/ filed)

 (setq filed (getfiled "Select IO Spreadsheet" (getvar "DwgPrefix") "" 10))
 (set_tile "file" filed)
 (setq file filed))


(defun selectstamp ()          ;define function

  (setq dcl_id (load_dialog "dialogboxes.dcl"))    ;load dialog

  (if (not (new_dialog "stamps" dcl_id))      ;test for dialog

      ;not

    (exit))            ;exit if no dialog

  ;if
  (setq stamp "Approved")

  (action_tile "rb1" "(setq stamp \"Approved\")")    ;store hole type
  (action_tile "rb2" "(setq stamp \"AsBuilt\")")    ;store hole type
  (action_tile "rb3" "(setq stamp \"ForApproval\")")    ;store hole type
  (action_tile "rb4" "(setq stamp \"ForInfoOnly\")")    ;store hole type
  (action_tile "rb5" "(setq stamp \"Obsolete\")")    ;store hole type
  (action_tile "rb6" "(setq stamp \"Preliminary\")")    ;store hole type


  (action_tile
   "cancel"            ;if cancel button pressed
   "(done_dialog) (setq userclick nil)")    ;close dialog, set flag
    ;action_tile

  (action_tile
    "accept"            ;if O.K. pressed
    "(done_dialog)(setq userclick T)")      ;close dialog, set flag
  ;action tile

  (start_dialog)          ;start dialog

  (unload_dialog dcl_id)        ;unload

 (princ stamp))

;defun C:samp


(defun fileopen ()          ;define function

  (setq dcl_id (load_dialog "dialogboxes.dcl"))    ;load dialog

  (if (not (new_dialog "open" dcl_id))      ;test for dialog

      ;not

    (exit))            ;exit if no dialog

  ;if
  (action_tile "proj" "(setq proj $value)")


  (action_tile
   "cancel"            ;if cancel button pressed
   "(done_dialog) (setq userclick nil)")    ;close dialog, set flag
    ;action_tile

  (action_tile
    "accept"            ;if O.K. pressed
    "(done_dialog)(setq userclick T)")      ;close dialog, set flag
  ;action tile

  (start_dialog)          ;start dialog

  (unload_dialog dcl_id)        ;unload

 (princ stamp))

;defun C:samp

(defun revbox ()          ;define function

  (setq dcl_id (load_dialog "dialogboxes.dcl"))    ;load dialog

  (if (not (new_dialog "rev" dcl_id))      ;test for dialog

      ;not

    (exit))            ;exit if no dialog

  ;if
  (action_tile "revv" "(setq revv $value)")


  (action_tile
   "cancel"            ;if cancel button pressed
   "(done_dialog) (setq userclick nil)")    ;close dialog, set flag
    ;action_tile

  (action_tile
    "accept"            ;if O.K. pressed
    "(done_dialog)(setq userclick T)")      ;close dialog, set flag
  ;action tile

  (start_dialog)          ;start dialog

  (unload_dialog dcl_id)        ;unload

 (princ))

;defun revupdate
