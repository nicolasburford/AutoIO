(defun plinputs ()
  ;(setq file (getfiled "Test" "C:\\" "" 10))

   ;(setq conso "P_PLC")
  (setq cnum "1771")
  (setq cnum2 "1794")
  ;(setq proj "5964")
  ;(setq name2 "055")

  (setq data (getexceldata (findfile file) (strcat conso " IO")))
  (setq cnt (length data))
  (if (= (nth 0 data) "SheetFail")
    (progn
      (alert (strcat "Sheet " conso " IO not found."))
     (setq cnt 0)))


  ;(setq data (getexcel file (strcat conso " IO") "G2"))
  ;(setq nrows (getdata data 1 6))
  ;(setq data (getexcel file (strcat conso " IO") (strcat "H" nrows)))

  (setq slots '())

  (setq i 0)
  (setq k 0)
  (setq flag 1)
  (while (< i cnt)

    (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (substr (getdata data i 0) 1 4) cnum2)(= (getdata data i 0) "EMPTY"))
      (progn
       (setq slot (getdata data i 0))
       (setq slots (append slots (list slot)))

      ;check if sheet exists and if correct module
      ;if sheet doesnt exist create, if not correct module, delete
      ;if correct module check if it has input data if it does
      ;replace with updated template version. if not leave untouched
       (if (AND (> k 1)(/= slot "EMPTY"))
        (progn
          (if (AND (/= (substr (getdata data (+ i 1) 0) 1 4) cnum)(/= (getdata data (+ i 1) 0) "EMPTY")(/= (getdata data (+ i 1) 0) "ControlLogix.0")(not (vl-string-search "Analog" (getdata data i 1)))) ;if it has inputs
            (progn
              (if (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))));if block exists in correct layout
               (progn
                 (setq pname (rtos (+ k 8) 2 0))
                 (delsymb pname)) ;delete that layouts symbols

               (addlayout slot (+ k 8)))

              (plinputsh))

            (progn
              (if (not (OR (ssget "X" (list (cons 2 slot)(cons 410 (rtos (+ k 8) 2 0))))(ssget "X" (list (cons 2 (strcat slot "a"))(cons 410 (rtos (+ k 8) 2 0))))))
               (addlayout slot (+ k 8)))))))





       (setq k (+ 1 k))))


    (setq i (+ 1 i)))


  (tab_sort)
  (crossref)
  (srxTEXT "Substring" "YYYY" proj "All")  ;Finding and Replacing
  (srxTEXT "Substring" "055" name2 "All")
  (srxTEXT "Substring" "1234" conso "All"))



(defun plinputsh ()
  ;(command "ctab" pname)	;make that sheet active
  ;(command "ZOOM" "EXTENTS")
  (setvar 'osmode 0) ;turn osnap off

  ;(setq obj (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0)))))
  ;(srxTEXT "Substring" "XX" (formatnum (- k 2)) obj)

  (setq lss (ssget "X" (list (cons 0 "INSERT")(cons 66 1)(cons 410 pname)))) ;all blocks with attributes in layout
  (setq i (+ i 1))

  (setq iocnt 0)
  (if (= (substr (nth k slots) 6 1) "I") ;If its an input module
    (setq topt '(1.0 21.5)))

  (if (= (substr (nth k slots) 6 1) "O") ;If its an output module
    (setq topt '(11.0 21.5)))

  (setq flag 1)
  (while (/= flag 0)
    (setq ks (rtos iocnt 2 0))
    (setq resindex (getindex lss ks))


    ;Formatting Data
    (if (= (getdata data i 2) "")
      (setq text "SPARE")
      (setq text (strcat (getdata data i 2) " " (getdata data i 3) " " (getdata data i 4) " " (getdata data i 5) " " (getdata data i 6))))

    (if (= (getdata data i 1) "")
      (setq symb "")
      (setq symb (getdata data i 1)))

    (if (= (getdata data i 8) "")
      (setq refnum "")
      (setq refnum (getdata data i 8)))

    (if (= (getdata data i 7) "")
      (setq symbname "")
      (progn
       (setq symbname (getdata data i 7))

       (setq col "")

       (if (= (substr symbname 1 4) "LAMP")
         (progn
           (if (= (substr symbname 5 1) " ") ;lamp
             (progn
              (setq col (substr symbname 6 1))
              (setq symbname "LAMP")))


           (if (= (substr symbname 7 1) " ") ;lampwr
             (progn
              (setq col (substr symbname 8 1))
              (setq symbname "LAMPWR")))))



  ;if
       (if (findfile (strcat symbname ".dwg"))
         (progn
           (if (= refnum "")
             (progn
              (setq point (cdr (assoc 10 (entget (ssname lss resindex)))))
              (insblock pname point (findfile (strcat symbname ".dwg")))
              (if (or (= symbname "3PPP")(= symbname "2PPP")(= symbname "PBNOWR"))
                (progn
                  (vla-explode iblk)
                  (vla-delete iblk))))



        ;(command ".-insert" symbname topt "" "" "" "" "" "" "" "")
             (progn
              (setq point (cdr (assoc 10 (entget (ssname lss resindex)))))
              (insblock pname point (findfile (strcat symbname ".dwg")))
              (if (or (= symbname "3PPP")(= symbname "2PPP")(= symbname "PBNOWR")(= symbname "LAMPWR"))
                (progn
                  (vla-explode iblk)
                  (vla-delete iblk)
                  (setq ss (ssget "X" (list (cons -4 "<or")(cons 2 "inref1")(cons 2 "inref2")(cons 2 "outref1")(cons -4 "or>")(cons 410 pname))))
                  (setattributevalue (vlax-ename->vla-object (ssname ss (findref))) "REF" refnum))))))





         (alert (strcat "\"" symbname "\"" " block not found in symbol library.")))))

      ;else


    (setq ks (rtos iocnt 2 0))
    (if (= (substr symbname 1 4) "LAMP")
      (progn
       (setq d 0)
       (if (AND (= (substr symbname 1 6) "LAMPWR")(= refnum ""))
         (progn
           (vla-explode iblk)
           (vla-delete iblk)))

    ;(command "_.explode" "L")

       (setq ss (ssget "X" (list (cons -4 "<or")(cons 2 "LAMPWRbl")(cons 2 "LAMP")(cons -4 "or>")(cons 410 pname))))
       (while (/= "" (getattributevalue (vlax-ename->vla-object (ssname ss d)) "LAMP"))
         (setq d (+ 1 d)))

       (setattributevalue (vlax-ename->vla-object (ssname ss d)) "LAMP" col)))



    (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "IONUM" (getdata data i 0))
    (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "WIRENO" (getdata data i 0))
    (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "DESC" text)
    (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "SYMBOL" symb)
    ;(setdataatt lss ks text symb) ;sets attributes for correct block
    ;(command "._attedit" "_N" "_N" obj "DESC" ks ks text)
    ;(command "._attedit" "_N" "_N" obj "SYMBOL" ks ks symb)

    ;Increasing Counts
    (setq iocnt (+ iocnt 1))
    (setq i (+ i 1))

    ;Loop condition
    (if (= i cnt)
      (setq flag 0)
      (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (substr (getdata data i 0) 1 4) cnum2)(= (getdata data i 0) "EMPTY"))(setq flag 0))))


  (setq i (- i 1))
  (setq flag 1))
