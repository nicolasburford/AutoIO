;;This file contains all of the functions necessary for ControlLogix AutoIO.

;function that sorts the layouts so that they are in increasing order
(defun TAB_SORT ( / tabs lay item go index test1 test2)
  (setq tabs
   (vla-get-Layouts
    (vla-get-Activedocument (vlax-get-acad-object)))

   lay (layoutlist)
   index 1)

 (while (/= index 0)
  (setq index 0)
  (foreach n lay
   (setq item (vla-item tabs n))
   (if (/= (vla-get-TabOrder item) (1+ (vl-position n lay)))
       (progn
          (setq test1 (- (length lay) (setq test2 (1+ (vl-position n lay)))))
          (cond
             ((> test1 test2)
              (if (> test1 index) (setq go n index test1)))

             ((> test2 index)
              (setq go n index test2))))))





  (if (/= index 0)
     (vla-put-TabOrder (vla-item tabs go) (1+ (vl-position go lay)))))


 (princ))


;this function will change all of most occuring stamps
(defun C:addstamp ()
  (selectstamp)
  (setq topt '(34 0))
  (if (= userclick T)
    (foreach layout (layoutlist)
      (setq i 0 pt nil)
      (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 66 1) (cons 410 layout))))
      (repeat (sslength ss)
        (setq valobj (vlax-ename->vla-object (ssname ss i)))
        (setq attlist (getattributes valobj))
        (setq upatt (cdr (assoc "DATE/TIME" attlist)))
        (if (AND (/= upatt nil) (= (length attlist) 1))
          (progn
            (setq pt (cdr (assoc 10 (entget (ssname ss i)))))
            (vla-delete valobj)
          )
        )
        (setq i (+ i 1))
      )
      (if (= pt nil)
        (insblock layout topt (strcat stamp ".dwg"))
        (insblock layout pt (strcat stamp ".dwg"))
      )
      (setattributevalue iblk "DATE/TIME" (todayLong))
    )
  )
)



;; Get Attribute Value  -  Lee Mac
;; Returns the value held by the specified tag within the supplied block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; Returns: [str] Attribute value, else nil if tag is not found.

(defun getattributevalue ( blk tag)
    (setq tag (strcase tag))
    (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)))


;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.
(defun setattributevalue ( blk tag val)
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att)
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)))


        (vlax-invoke blk 'getattributes)))

(defun getwidth ( ss / ent obj HeadHeight Height Width)
  ;(setq ss (ssget))
  (setq en (tblobjname "block" (cdr (assoc 2 (entget ss)))))
  (setq point (cdr (assoc 10 (entget en))))
  (setq min (nth 0 point))
  (setq max (nth 0 point))
  (while en
    (setq point1 (cdr (assoc 10 (entget en))))
    (setq point2 (cdr (assoc 11 (entget en))))
    (if (/= point1 nil)
      (progn
        (if (< (nth 0 point1) min)
            (setq min (nth 0 point1)))
        (if (> (nth 0 point1) max)
            (setq max (nth 0 point1)))
      )
    )
    (if (/= point2 nil)
      (progn
        (if (< (nth 0 point2) min)
            (setq min (nth 0 point2)))
        (if (> (nth 0 point2) max)
            (setq max (nth 0 point2)))
      )
    )
    (setq en (entnext en))
  )
  (- max min)
)

;; Get Attributes  -  Lee Mac
;; Returns an association list of attributes present in the supplied block.
;; blk - [vla] VLA Block Reference Object
;; Returns: [lst] Association list of ((<Tag> . <Value>) ... )

(defun getattributes ( blk)
    (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)))



;this function adds preliminary stamps to all layouts
(defun prelim ()

  (setvar 'osmode 0)      ;osnap off
  (setq topt '(31.75 6.5625))
  (setq topt2 '(22.1875 2.5))

  (vlax-for x (vla-get-layouts
               (vla-get-activedocument (vlax-get-acad-object)))

    ;;add a loop to only delete the ones wanted
    (setq modname (vla-get-name x))

    (if  (/= modname "Model")
      (progn
       (command "ctab" modname)  ;go to that sheet
       (command "ZOOM" "EXTENTS")
       (if (= modname "09")
           (command  "._insert" "Preliminary" topt2 "" "" "" (today))
           (command  "._insert" "Preliminary" topt "" "" "" (today)))))))




(defun todayLong ( / d yr mo day)
  (setq d (rtos (getvar "CDATE") 2 6)
       ;get the date and time and convert to text

            yr (substr d 3 2)
  	  ;extract the year

            mo (substr d 5 2)
  	  ;extract the month

           day (substr d 7 2)
  	 ;extract the day

       )
   (setq mos '("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

    (strcat (nth (atoi mo) mos) " " (itoa (atoi day)) ", 20" yr)
)

;this function returns the current date
(defun today ( / d yr mo day)
;define the function and declare all variabled local

     (setq d (rtos (getvar "CDATE") 2 6)
     ;get the date and time and convert to text

          yr (substr d 3 2)
    ;extract the year

          mo (substr d 5 2)
    ;extract the month

         day (substr d 7 2))
   ;extract the day

     ;setq

     (strcat mo "/" day "/" yr))
     ;string 'em together

(defun sublst ( lst idx len )
    (cond
        (   (null lst) nil)
        (   (< 0  idx) (sublst (cdr lst) (1- idx) len))
        (   (null len) lst)
        (   (< 0  len) (cons (car lst) (sublst (cdr lst) idx (1- len))))
    )
)

;getdata is used to simplify data indexing
(defun getdata (data row col)
  (nth col (nth row data)))


;first version of referencing (see reference file)
(defun crossref ()
  (setq ssi (ssget "X" (list (cons -4 "<or")(cons 2 "inref1")(cons 2 "inref2")(cons -4 "or>"))))
  (setq sso (ssget "X" (list (cons 2 "outref1"))))

  (setq i 0)
  (if (and (/= ssi nil)(/= sso nil))
    (progn
      (repeat (sslength ssi)
       (setq o 0)
       (repeat (sslength sso)
         (setq iref (getattributevalue (vlax-ename->vla-object (ssname ssi i)) "REF"))
         (setq oref (getattributevalue (vlax-ename->vla-object (ssname sso o)) "REF"))
         (if (= iref oref)
           (progn
             (setq sheet (cdr (assoc 410 (entget (ssname ssi i)))))
             (setq point (cdr (assoc 10 (entget (ssname ssi i)))))
             (if (< (nth 0 point) 14)
              (setq bline 45)
              (setq bline 90))

             (setq line (fix (- bline (* 2 (nth 1 point)))))
             (setq oref (strcat "REF DWG YYYY-055-1234, SHEET " sheet " LINE " (rtos (+ line 1) 2 0)))
             (setq sheet (cdr (assoc 410 (entget (ssname sso o)))))
             (setq point (cdr (assoc 10 (entget (ssname sso o)))))
             (if (< (nth 0 point) 14)
              (setq bline 45)
              (setq bline 90))

             (setq line (fix (- bline (* 2 (nth 1 point)))))
             (setq iref (strcat "REF DWG YYYY-055-1234, SHEET " sheet " LINE " (rtos (+ line 1) 2 0)))

             (setattributevalue (vlax-ename->vla-object (ssname ssi i)) "REF" iref)
             (setattributevalue (vlax-ename->vla-object (ssname sso o)) "REF" oref)))


         (setq o (+ o 1)))

       (setq i (+ i 1))))))





;returns whether there is a number in a given string
(defun numbsinstring (str)
  ;; Removes the non-numeric characters from a string
  (vl-list->string
   (vl-remove-if-not 'num-char-p
                     (vl-string->list str))))
(defun num-char-p (char)
  ;; Does (chr num) represent a numeric character (0...9)?
 (< 46 char 58))


;Find a block with a given index
(defun getindex (lss index)
  (setq a 0)
  (setq resindex nil)
  (repeat (sslength lss)
    (setq attlist (getattributes (vlax-ename->vla-object (ssname lss a))))
    (setq indexatt (cdr (assoc "INDEX" attlist))) ;description attribute -nil if not
    (if (= indexatt index)
     (setq resindex a))

    (setq a (+ a 1)))

  (setq a resindex))


;;;Insert a block into a certain layout
;;;all at 0,0,0 & 0 deg. rotation

(defun insblock (laystr inspt blname)
  (setq laylay (vlax-get (vla-item (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) laystr) 'block))
  (setq pt (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (setq pt (vlax-safearray-fill pt (list (nth 0 inspt) (nth 1 inspt) 0)))
  (setq iblk (vla-insertblock laylay pt blname 1.0 1.0 1.0 0.0))) ;xcale, yscale, zscale, rotation

  ;(setq explodedObjects (vlax-variant-value (vla-Explode iblk)))
  ;(vlax-safearray-get-u-bound explodedObjects 1)
  ;(vlax-safearray-get-element explodedObjects 0)
  ;(vla-delete iblk)
  ;(vlax-vla-object->ename iblk)


(defun exportblks ()
  (foreach layout (layoutlist)
    (command "ctab" layout)
    (setq ss (ssget "X" (list (cons 410 layout))))
    (command "export" (strcat "\\\\usnr.com\\data\\Eng\\SA\\AutoCad\\Custom\\AutoIO Tools\\Development\\AutoIO\\Templates\\Blocks\\" layout "lay.dwg") "" '(0 0) ss "")
    (command "undo" "")))


(defun importblkss ()
  (foreach layout (layoutlist)
      (insblock layout '(0 0) (findfile (strcat layout "lay.dwg")))
    (vla-explode iblk)
    (vla-delete iblk)))



;add a sheet from templates
(defun addlayout (modname num slotnum)

 (command "-layout" "template" tfile modname "") ;will continue even if slot not found

 (setq modname (vl-list->string
                (vl-remove
                  32
                 (vl-string->list modname))))

  ;error handling will let user know if module not found
 (if (member modname (layoutlist))
   (progn
     (if (member (rtos num 2 0) (layoutlist))
      (setq pname (strcat (rtos num 2 0) "a"))
      (setq pname (rtos num 2 0)))

     (princ num)
     (command "_.layout" "rename" modname pname)

      ;Updating Corner Block information
     (if (member "09" (layoutlist))
      (progn
        (setq ss (ssget "X" (list (cons 2 "BorderArchD")(cons 410 "09"))))
        (setq blockRef9 (vlax-ename->vla-object (ssname ss 0)))
        (setq ss (ssget "X" (list (cons 2 "BorderArchD")(cons 410 pname))))
        (setq blockRef (vlax-ename->vla-object (ssname ss 0)))
        (setq ss (ssget "_X" (list (cons 2 "Preliminary")(cons 410 pname))))
        ;(setq blockRefs (vlax-ename->vla-object (ssname ss 0)))

    ;Getting info from sheet 9
        (setq desctag (getattributevalue blockRef9 "DWG_DESCRIPTION"))
        (setq comptag (getattributevalue blockRef9 "CLIENT_NAME"))
        (setq locatag (getattributevalue blockRef9 "CLIENT_LOCATION"))
        (setq draftag (getattributevalue blockRef9 "DRAFTER"))
        (setq letttag (getattributevalue blockRef9 "LETTER_SECTION"))

    ;Setting Tags
        (setattributevalue blockRef "PG" (rtos num 2 0))
        (setattributevalue blockRef "DWG_DESCRIPTION" desctag)
        (setattributevalue blockRef "CLIENT_NAME" comptag)
        (setattributevalue blockRef "LETTER_SECTION" letttag)
        (setattributevalue blockRef "CLIENT_LOCATION" locatag)
        (setattributevalue blockRef "DRAFTER" draftag)
        (setattributevalue blockRef "DATE" (today))
        ;(setattributevalue blockRefs "DATE/TIME" (today))
        )
        (progn
          (setq ss (ssget "X" (list (cons 2 "BorderArchD")(cons 410 pname))))
          (setq blockRef (vlax-ename->vla-object (ssname ss 0)))
          (setattributevalue blockRef "PG" (rtos num 2 0))
          (setattributevalue blockRef "DATE" (today))
        )
    )


     (setq obj (ssget "X" (list (cons 410 pname)))) ;everything in layout
     (srxTEXT "Substring" "ZZ" slotnum obj))

   (alert (strcat "\"" modname "\"" " Module not found in template."))))



(defun formatnum (num)
  (if (< num 10)
    (setq numstr (strcat "0" (rtos num 2 0)))
    (setq numstr (rtos num 2 0))))



;find most common stamp
(defun currentstamps ()
  (setq stamplist '("Approved" "AsBuilt" "DoNotCopy" "Existing" "ForApproval" "ForInfoOnly" "Obsolete" "Preliminary" "Standard" "TBC"))
  (setq maxrep 0)
  (foreach stamp stamplist
    (if (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 stamp) (cons 66 1))))
      (progn
       (if (> (sslength ss) maxrep)
         (progn
           (setq maxrep (sslength ss))
           (setq mstamp stamp))))))





  (if (ssget "_X" (list (cons 0 "INSERT") (cons 2 "Revised") (cons 66 1)))
    (setq rflag "T")
    (setq rflag "F"))

  (princ (list mstamp rflag)))


(defun delsymb (pname)
  (setq ss (ssget "X" (list (cons 0 "INSERT")(cons 66 1)(cons 410 pname))))
  (setq a 0)
  (repeat (sslength ss)
    (setq attlist (getattributes (vlax-ename->vla-object (ssname ss a))))
    (setq upatt (cdr (assoc "UPDATE" attlist))) ;description attribute -nil if not
    (if (= upatt "T")
      (vla-delete (vlax-ename->vla-object (ssname ss a))))

    (setq a (+ a 1))))



(defun explodesel (ent nameblk / object mySet object_list)
  ;(setq mySet(ssadd))
  (setq object_list
         (vlax-safearray->list
           (vlax-variant-value
             (vla-explode ent))))



  (vla-delete ent)
;;;  (foreach object object_list
;;;    (setq mySet(ssadd (vlax-vla-object->ename object) mySet))
;;;  )

  (setq re 0)
  (foreach object object_list
    (if (= nameblk (cdr (assoc 2 (entget (vlax-vla-object->ename object)))))
      (setq output object))))




;delete folder
(defun delFolder (folder)
 (vl-load-com)
 (setq fso (vlax-create-object "Scripting.FileSystemObject"))
 (vlax-invoke-method fso 'DeleteFolder folder  :vlax-false)
 (vlax-release-object fso))


;make a folder
(defun MakeDir (Pathname$ / Drive$ Folder$ Folders@)
  (setq Pathname$ (vl-filename-directory (strcat Pathname$ "\\")))
  (while (/= Drive$ Pathname$)
    (setq Folder$ (substr Pathname$ (1+ (strlen (vl-filename-directory Pathname$)))))
    (if (= (substr Folder$ 1 1) "\\")
      (setq Folder$ (substr Folder$ 2)))
    ;if
    (setq Folders@ (cons Folder$ Folders@)
          Drive$ Pathname$
          Pathname$ (vl-filename-directory Pathname$)))
    ;setq
  ;while
  (foreach Folder$ (cdr Folders@)
    (vl-mkdir (strcat Pathname$ Folder$))
    (setq Pathname$ (strcat Pathname$ Folder$ "\\"))))
  ;foreach
;defun MakeDir

;copy a folder
(defun LM:Copy ( target dest / fso fob result ) (vl-load-com)
  (if
    (and
      (setq target (findfile target))
      (setq dest   (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dest)) "\\"))
      (or (vl-file-directory-p dest) (vl-mkdir dest))
      (setq fso    (vlax-create-object "Scripting.FileSystemObject"))
      (setq fob    (vlax-invoke fso (if (vl-file-directory-p target) 'getfolder 'getfile) target)))

    (setq result (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-invoke (list fob 'Copy dest))))))

  (if fso (vlax-release-object  fso))
  (if fob (vlax-release-object  fob))
  result)


(defun findref()
  (setq re 0)
  (setq reflag 0)
  (while (= reflag 0)
    (setq point2 (cdr (assoc 10 (entget (ssname ss re)))))
    (if (AND (= (/ (atoi (rtos (* 2 (nth 1 point2)) 2 0)) 2.0)(atof (rtos (nth 1 point) 2 2)))(> (* (- (nth 0 point2) 14)(- (atof (rtos (nth 0 point) 2 2)) 14)) 0))
      (progn
       (setq reflag 1)
       (setq re (- re 1))))


    (setq re (+ re 1))))



(defun openusnr ()
  (fileopen)
  (if userclick (USNRFile proj)))


;extract io from current drawing
;and save to csv file
(defun extract ()
  (getvar "dwgprefix")
  (setq ss (ssget "_X" (list (cons 0 "INSERT")(cons 66 1))))
  (setq file (open (strcat (getvar "dwgprefix") (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)) ".csv") "w"))

  (setq i 0)
  (repeat (sslength ss)
    (if (setq ionum (getattributevalue (vlax-ename->vla-object (ssname ss i)) "IONUM"))
      (progn
       (setq symb (getattributevalue (vlax-ename->vla-object (ssname ss i)) "SYMBOL"))
       (setq desc (getattributevalue (vlax-ename->vla-object (ssname ss i)) "DESC"))
       (if (= 0 (vl-string-search "\\" desc 0))
         (setq desc (substr desc (+ 2 (vl-string-search ";" desc 0)))))

       (write-line (strcat ionum ", " symb ", " desc) file)))
  ;(princ iref)


    (setq i (+ i 1)))

  (close file)
  (alert (strcat (strcat (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4)) ".csv") " created successfully.")))


;update all revision blocks
(defun revupdate ()
 (revbox)
 (setq ss (ssget "X" (list (cons 2 "BorderArchD"))))
 (setq i 0)
 (repeat (sslength ss)
  (setq blockRef (vlax-ename->vla-object (ssname ss i)))
  (setattributevalue blockRef "REV" revv)
  (setq i (+ i 1)))

 (princ))


(defun importlayout (modname)
  (setq firstlay (nth 0 (layoutlist)))
  (setq ss (ssget "X" (list (cons 2 "BorderArchD")(cons 410 firstlay))))
  (if ss
   (setq proj (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "LETTER_SECTION")))


  ;(setq tfile (strcat "C:\\Users\\nicolas.burford\\Documents\\Development\\AutoCAD\\HexTemp\\" modname "tmp.dwg"))
  (setq tfile (strcat "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\ELECTRICAL LIBRARY\\HexTemp\\" modname "tmp.dwg"))
  (command "-layout" "template" tfile modname) ;will continue even if slot not found
  (princ "imported\n")
;Updating Corner Block information
 (if ss
   (progn
     (setq ss (ssget "X" (list (cons 2 "BorderArchD")(cons 410 firstlay))))
     (setq blockRef9 (vlax-ename->vla-object (ssname ss 0)))
     (setq ss (ssget "X" (list (cons 2 "BorderArchD")(cons 410 modname))))
     (setq blockRef (vlax-ename->vla-object (ssname ss 0)))

     (if (setq ss (ssget "X" (list (cons 2 "Preliminary")(cons 410 modname))))
       (progn
         (setq blockRefs (vlax-ename->vla-object (ssname ss 0)))
         (setattributevalue blockRefs "DATE/TIME" (today))))



    ;Getting info from sheet 9
     (setq desctag (getattributevalue blockRef9 "DWG_DESCRIPTION"))
     (setq comptag (getattributevalue blockRef9 "CLIENT_NAME"))
     (setq locatag (getattributevalue blockRef9 "CLIENT_LOCATION"))
     (setq draftag (getattributevalue blockRef9 "DRAFTER"))
     (setq letttag (getattributevalue blockRef9 "LETTER_SECTION"))

    ;Setting Tags
     (setattributevalue blockRef "DWG_DESCRIPTION" desctag)
     (setattributevalue blockRef "CLIENT_NAME" comptag)
     (setattributevalue blockRef "LETTER_SECTION" letttag)
     (setattributevalue blockRef "CLIENT_LOCATION" locatag)
     (setattributevalue blockRef "DRAFTER" draftag)
     (setattributevalue blockRef "DATE" (today))))



 (princ)
 (setq ss (ssget "X" (list (cons 410 modname))))
 (if proj
   (progn
    (srxTEXT "Substring" "1234" (substr proj (- (strlen proj) 3) 4) ss)
    (srxTEXT "Substring" "YYYY" (substr proj 1 4) ss)
    (srxTEXT "Substring"  "055" (substr proj 6 3) ss)))


 (princ)
 (command "ctab" modname)
 (command "ZOOM" "EXTENTS"))


;ELECTRICAL LIBRARY FUNCTION
;used to add a block to library
(defun addtolib (hexnum strpth)
  (setq ss (ssget))
  (setq pt (getpoint "\nSelect the insertion point:"))
  (command "export" strpth "" pt ss "")
  (command "UNDO" "")
  (setq wsh (vlax-create-object "Wscript.Shell"))
  (vlax-invoke wsh 'AppActivate "USNR Electrical Library")
  (vlax-release-object wsh))
  ;(alert "Block Created")

  ;(princ 1)


;ELECTRICAL LIBRARY FUNCTION
;used to add a template to library
(defun maketemp(hexnum opth olay)
  ;(setq tfile (strcat "C:\\Users\\nicolas.burford\\Documents\\Development\\AutoCAD\\HexTemp\\" hexnum "tmp.dwg"))
  (setq tfile (strcat "\\\\usnr.com\\data\\Eng\\Electrical\\New Electrical Design\\Electrical\\AutoIO Tools\\Blocks\\ELECTRICAL LIBRARY\\HexTemp\\" hexnum "tmp.dwg"))

  (command "-layout" "template" opth olay)
  (setq ss (ssget "X" (list (cons 2 "BorderArchD"))))
  (setq proj (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "LETTER_SECTION"))
  (setattributevalue (vlax-ename->vla-object (ssname ss 0)) "PG" "")
  (setq ss (ssget "X"))
  (srxTEXT "Substring" (substr proj (- (strlen proj) 3) 4) "1234" ss)
  (srxTEXT "Substring" (substr proj 1 4) "YYYY" ss)
  (srxTEXT "Substring" (substr proj 6 3) "055" ss))


;start the electrical library app
(defun startlib  ()
  (if (setq wsh (vlax-create-object "wscript.shell"))
    (progn
      (if (= (vlax-invoke wsh 'appactivate "USNR Electrical Library") 0)
       (launchlib))

      (vlax-release-object wsh)))


  (princ))


;function can be used to extract io from multiple drawings into one file
;(vb required to run it on several drawings)
(defun appendextract (path)
  (setq ss (ssget "_X" (list (cons 0 "INSERT")(cons 66 1))))
  (setq file (open path "a"))

  (setq i 0)
  (repeat (sslength ss)
    (if (setq ionum (getattributevalue (vlax-ename->vla-object (ssname ss i)) "IONUM"))
      (progn
       (setq symb (getattributevalue (vlax-ename->vla-object (ssname ss i)) "SYMBOL"))
       (setq desc (getattributevalue (vlax-ename->vla-object (ssname ss i)) "DESC"))
       (if (= 0 (vl-string-search "\\" desc 0))
         (setq desc (substr desc (+ 2 (vl-string-search ";" desc 0)))))

       (if (/= desc "SPARE")
        (write-line (strcat ionum "," symb "," desc) file))))

  ;(princ iref)


    (setq i (+ i 1)))

  (close file))


(princ)
