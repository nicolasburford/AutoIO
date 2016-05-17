
;;fplace - Builds the rack and text in sheet 9.
(defun fplace (slots)

  ;;Selecting sheet 9, autosnap off, initial point conditions

  (setvar 'osmode 0)
  (setq topt (list 5.0 17.5))

  (setq pt1 '(34.0 0.0))
  ;;FULL WINDOW POINTS
  (setq pt2 '(0.0 23.0))
  (setq i 0)
  (if (findfile (strcat (nth 0 slots) "bl.dwg"))
    (progn
      (insblock "09" topt (findfile (strcat (nth 0 slots) "bl.dwg")))
      (vla-explode iblk)
      (vla-delete iblk))

    (alert (strcat slot " block not found.")))

  ;;inserting each component
  (foreach slot  slots
    (if  (> i 1)
      (progn
       (if (findfile (strcat slot "bl.dwg"))
         (progn
           (insblock "09" topt (findfile (strcat slot "bl.dwg")))
           (vla-explode iblk)
           (vla-delete iblk))

         (alert (strcat slot " block not found.")))



       (setq obj (ssget "X" (list (cons 410 "09"))))
       (srxTEXT "Substring" "X1" (rtos (+ i 8) 2 0) obj)

       (setq topt (list (+ 3.6875 (nth 0 topt)) (nth 1 topt)))))


    (setq i (+ i 1))
    (if  (and (> i 1) (/= i (length slots)))
      ;;if not at the end of the rack, insert connection
      (progn
       (if (AND (OR (= (substr (nth i slots) 8 2) "32")
                 (= (strlen (nth i slots)) 15))

            (OR (= (substr (nth (- i 1) slots) 8 2) "32")
                (= (strlen (nth (- i 1) slots)) 15)))


         (insblock "09" topt (findfile "connect2bl.dwg")))

       (if (AND (AND (= (strlen (nth i slots)) 9)
                 (= (substr (nth i slots) 8 2) "16"))

            (AND (= (strlen (nth (- i 1) slots)) 9)
                 (= (substr (nth (- i 1) slots) 8 2) "16")))


         (insblock "09" topt (findfile "connectbl.dwg")))))))






(defun finputs ()
  ;(setq file "Master Test.xls")

  ;(setq tfile "C:\\Drafting\\Custom\\AutoIO\\Templates\\1794_Flex_AutoIO_Template.dwg") ;TEMPLATE FILE LOCATION
  ;(setq conso "E1F1")
  (setq cnum "1794")
  ;(setq proj "5964")
  ;(setq name2 "055")

;;;  (setq data (getexcel file (strcat conso " IO") "G2"))
;;;  (setq nrows (getdata data 1 6))
;;;  (setq data (getexcel file (strcat conso " IO") (strcat "H" nrows)))
;;;  (setq cnt (atoi nrows))
  (setq data (getexceldata (findfile file) (strcat conso " IO")))
  (setq cnt (length data))
  (if (= (nth 0 data) "SheetFail")
    (progn
      (alert (strcat "Sheet " conso " IO not found."))
      (setq cnt 0)))



  (setq slots '())

  (setq i 0)
  (setq k 0)
  (while (< i cnt)
    (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (getdata data i 0) "EMPTY"))
      (progn
       (setq slot (getdata data i 0))
       (if (= (substr slot (strlen slot) 1) " ")
         (setq slot (substr slot 1 (- (strlen slot) 1))))

       (setq slots (append slots (list slot)))

      ;check if sheet exists and if correct module
      ;if sheet doesnt exist create, if not correct module, delete
      ;if correct module check if it has input data if it does
      ;replace with updated template version. if not leave untouched
       (if (AND (> k 1)(/= slot "EMPTY"))
        (progn
          (if (AND (/= (substr (getdata data (+ i 1) 0) 1 4) cnum)(/= (getdata data (+ i 1) 0) "")(/= (getdata data (+ i 1) 0) "EMPTY")(/= (getdata data (+ i 1) 0) "ControlLogix.0")(not (vl-string-search "Analog" (getdata data i 1)))) ;if it has inputs
            (progn
              (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
               (progn
                 (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
                   (setq pname (rtos (+ k 8) 2 0))
                   (addlayout slot (+ k 8))))


               (addlayout slot (+ k 8)))

              (setq iocnt 0)
              (inputsh))

            (progn
              (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
               (progn
                 (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
                   (setq pname (rtos (+ k 8) 2 0))
                   (addlayout slot (+ k 8))))


               (addlayout slot (+ k 8)))))))





       (setq k (+ 1 k))))


    (setq i (+ 1 i))))
