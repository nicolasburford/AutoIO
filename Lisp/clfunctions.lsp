(defun clinput ()

  (setq cnum "1756")
  ;Reading data from excel
  (setq data (getexceldata (findfile file) (strcat conso " IO")))
  (setq cnt (length data))
  (if (= (nth 0 data) "SheetFail")
    (progn
      (alert (strcat "Sheet " conso " IO not found."))
     (setq cnt 0)))



  (setq slots '())

  (setq i 0)
  (setq k 0)
  (setq flag 1)

  ;Running down all rows
  (while (< i cnt)
    ;Find a slot
    (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (getdata data i 0) "EMPTY"))
      (progn
       (setq slot (getdata data i 0))
       (setq slots (append slots (list slot)))
       (setq slotnum (formatnum (- k 1)))
       (if (AND (> k 1)(/= slot "EMPTY")(/= slot "1756-N2"))
        (progn
          (if (< i (- cnt 1))
            (progn
     ;if slot has inputs
             (if (AND (/= (substr (getdata data (+ i 1) 0) 1 4) cnum)(/= (getdata data (+ i 1) 0) "EMPTY")(/= (getdata data (+ i 1) 0) "")(/= (getdata data (+ i 1) 0) "ControlLogix.0"));(not (vl-string-search "Analog" (getdata data i 1)))) ;if it has inputs
               (progn
        ;If a sheet exists on given page. If its the same module keep it if its different
        ;then add correct one
                 (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
                  (progn
                    (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
                      (setq pname (rtos (+ k 8) 2 0))

                      (addlayout slot (+ k 8) slotnum)))


                  (addlayout slot (+ k 8) slotnum))

        ;update io
                 (setq iocnt 0)
                 (inputsh))

               (progn
        ;If it doesnt have inputs then do the same but never update inputs
                 (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
                  (progn
                    (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
                      (setq pname (rtos (+ k 8) 2 0))
                      (addlayout slot (+ k 8) slotnum)))


                  (addlayout slot (+ k 8) slotnum)))))




            (progn
              (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
               (progn
                 (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
                   (setq pname (rtos (+ k 8) 2 0))
                   (addlayout slot (+ k 8) slotnum)))


               (addlayout slot (+ k 8) slotnum))))))





       (setq k (+ 1 k))))


    (setq i (+ 1 i))))



;builds the rack
(defun placesh (slots / topt data)

  (setvar 'osmode 0)
  (setq topt (list 8.00 11.2693))

  (setq i 0)
  (foreach slot slots
    (if (= slot "EMPTY") (setq slot "1756-N2"))
    (if (= i 1)(setq slot (substr slot 1 (- (strlen slot) 2))))
    (if (findfile (strcat slot "wh.dwg"))
     (progn
       (insblock "09" topt (findfile (strcat slot "wh.dwg")))
      (setq desc1 (getattributevalue iblk "DESC1"))
      (setq desc2 (getattributevalue iblk "DESC2"))
      (vla-explode iblk)
      (vla-delete iblk)
      (if (/= desc1 nil)
        (vla-delete  (vlax-ename->vla-object (ssname (ssget "X" (list (cons 410 "09")(cons 0 "ATTDEF")(cons 2 "DESC1")))0)))
        (setq desc1 "XXXXXX"))

      (if (/= desc2 nil)
        (vla-delete  (vlax-ename->vla-object (ssname (ssget "X" (list (cons 410 "09")(cons 0 "ATTDEF")(cons 2 "DESC2")))0)))
        (if (= desc1 "XXXXXX")(setq desc2 "XXXXXX")(setq desc2 "")))

      (if (setq ss (ssget "X" (list (cons 410 "09")(cons 0 "TEXT")(cons 1 (strcat "X" (rtos (- i 2) 2 0) "1")))))
        (progn
          (setq ed (entget (ssname ss 0))) ;find
          (entmod (subst (cons 1 desc1)(assoc 1 ed) ed)))) ;replace


      (if (setq ss (ssget "X" (list (cons 410 "09")(cons 0 "TEXT")(cons 1 (strcat "X" (rtos (- i 2) 2 0) "2")))))
        (progn
          (setq ed (entget (ssname ss 0))) ;find
          (entmod (subst (cons 1 desc2)(assoc 1 ed) ed))))) ;replace



     (alert (strcat slot " block not found.")))


    (if (> i 1) (setq topt (list (+ 1.363 (nth 0 topt)) (nth 1 topt))))
    (if (= (nth 1 slots) "1756-A17/B")
      (if (or (= i 6) (= i 12)) (setq topt (list (+ 0.5969 (nth 0 topt)) (nth 1 topt)))))

    (if (= (nth 1 slots) "1756-A10/B")
      (if (= i 6) (setq topt (list (+ 0.7429 (nth 0 topt)) (nth 1 topt)))))

    (if (= (nth 1 slots) "1756-A13/B")
      (if (= i 7) (setq topt (list (+ 0.5969 (nth 0 topt)) (nth 1 topt)))))

    (setq i (+ i 1))))



;Main function used to update descriptions and add symbols to drawings
(defun inputsh ()

  (setvar 'osmode 0) ;turn osnap off

  (setq lss (ssget "X" (list (cons 0 "INSERT")(cons 66 1)(cons 410 pname)))) ;all blocks with attributes in layout
  (setq i (+ i 1))

  ;(setq iocnt 0)

  (setq flag 1)
  (while (/= flag 0)
    ;(setq lss (ssget "X" (list (cons 0 "INSERT")(cons 66 1)(cons 410 pname))))
    (setq ks (rtos iocnt 2 0))
    (setq resindex (getindex lss ks))
    (if (/= resindex nil) ;if the block exists in the layout
      (progn
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
         (progn
          (setq symbname "")
          (setq point (cdr (assoc 10 (entget (ssname lss resindex)))))
          (setq blname (cdr (assoc 2 (entget (ssname lss resindex)))))

  ;If the current symbol in the program does not match, then delete it.
          (if (setq ss (ssget "X" (list (cons 410 pname)(cons 10 point)(cons -4 "<not") (cons 2 blname) (cons -4 "not>"))))
            (progn
              (ssdel (ssname ss 0) lss)
              (setq temp (cdr (assoc 2 (entget (ssname ss 0)))))
              (vla-delete (vlax-ename->vla-object (ssname ss 0))))))



         (progn
          (setq symbname (getdata data i 7))
          (setq col "")

  ;;LAMP SPECIAL CASE
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

  ;;INTERPOSING RELAY SPECIAL CASE

          (if (= (substr symbname 1 3) "IPR")
            (progn
              (if (= (substr symbname 4 1) " ") ;lamp
                (progn
                 (setq col (substr symbname 5 (- (strlen symbname) 4)))
                 (setq symbname "IPR")))))






          (setq point (cdr (assoc 10 (entget (ssname lss resindex)))))
          (setq blname (cdr (assoc 2 (entget (ssname lss resindex)))))
          (if (setq ss (ssget "X" (list (cons 410 pname)(cons 10 point)(cons -4 "<not") (cons 2 blname) (cons -4 "not>"))))
            (progn
             (setq temp (cdr (assoc 2 (entget (ssname ss 0)))))
             (if (and (/= symbname temp)(/= temp (strcat symbname "bl")))
               (progn
                 (ssdel (ssname ss 0) lss)
                 (vla-delete (vlax-ename->vla-object (ssname ss 0)))

                 (if (setq blfile (findfile (strcat symbname ".dwg")))
                   (insblock pname point blfile)
                   (alert (strcat "\"" symbname "\"" " block not found in symbol library."))))))




            (progn
             (if (setq blfile (findfile (strcat symbname ".dwg")))
               (progn
                 (if (= symbname "IPR")
                   (progn
                    (setq subpoint (list (- (nth 0 point) 5.5625) (nth 1 point)(nth 2 point)))
                    (if (setq ss (ssget "X" (list (cons 410 pname)(cons 10 subpoint)(cons 2 "FUSE"))))
                      (vla-delete (vlax-ename->vla-object (ssname ss 0))))

                    (if (/= col "")
                      (progn
                        (if (setq col2 (findfile (strcat col ".dwg")))
                          (progn
                            (setq subpoint (list (nth 0 point) (- (nth 1 point) 0.5) (nth 2 point)))
                            (insblock pname subpoint col2))

                          (alert (strcat "\"" col "\"" " block not found in symbol library.")))))



                    (setq pwflg 0)
                    (if (< (+ i 1) cnt)
                      (progn
                       (if (/= (substr (getdata data (+ i 1) 7) 1 3) "IPR")
                         (setq pwflg 1)))


                      (progn
                        (if (= cnt (+ 1 i))
                          (setq pwflg 1))))



                    (if (= pwflg 1)
                      (progn
                        (insblock pname point (findfile "IPRPW.dwg"))
                        (vla-explode iblk)
                        (vla-delete iblk)))))


      ;if IPR progn
          ;if IPR
                 (insblock pname point blfile)
                 (if (and (= symbname "IPR")(/= (substr (getdata data (- i 1) 7) 1 3) "IPR"));if its the first one delete the intersection
                   (progn
                    (vla-explode iblk)
                    (vla-delete iblk)
                    (setq ss (ssget "X" (list (cons 410 pname)(cons 10 (list (- (nth 0 point) 9.125) (- (nth 1 point) 0.5) (nth 2 point))))))
                    (vla-delete (vlax-ename->vla-object (ssname ss 0))))))



               (alert (strcat "\"" symbname "\"" " block not found in symbol library.")))))))




      ;else


       (if (= (substr symbname 1 4) "LAMP")
         (progn
          (setq ss (ssget "X" (list (cons -4 "<or")(cons 2 "LAMPWR")(cons 2 "LAMP")(cons -4 "or>")(cons 10 point)(cons 410 pname))))
          (setattributevalue (vlax-ename->vla-object (ssname ss 0)) "LAMP" col)))


       (if (/= refnum "")
         (progn
          (setq ss (ssget "X" (list (cons 10 point)(cons 410 pname))))
          (if (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "SOURCEREFID")
              (setattributevalue (vlax-ename->vla-object (ssname ss 0)) "SOURCEREFID" refnum))

          (if (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "DESTREFID")
              (setattributevalue (vlax-ename->vla-object (ssname ss 0)) "DESTREFID" refnum))))



       (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "DESC" text)
       (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "SYMBOL" symb)

       (if (setq tmp (getattributevalue (vlax-ename->vla-object (ssname lss resindex)) "WIRENO"))
         (progn
           (setq tmp (strcat (substr (getdata data i 0) 1 4) (substr tmp 5 (strlen tmp))))
           (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "WIRENO" tmp)))


       (if (setq tmp (getattributevalue (vlax-ename->vla-object (ssname lss resindex)) "IONUM"))
         (progn
          (setq tmp (strcat (substr (getdata data i 0) 1 4) (substr tmp 5 (strlen tmp))))
          (setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "IONUM" tmp)))))


    ;(substr (getdata data i 0) 1 4)
    ;(setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "WIRENO" symb)
    ;(setattributevalue (vlax-ename->vla-object (ssname lss resindex)) "IONUM" symb)


    ;Increasing Counts
    (setq iocnt (+ iocnt 1))
    (setq i (+ i 1))

    ;Loop condition
    (if (= i cnt)
      (setq flag 0)
      (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (getdata data i 0) "EMPTY"))(setq flag 0))))

  ;Master While Loop
  (setq i (- i 1))
  (setq flag 1))


(defun c:setindeces (/ a)

  (setq lays (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))))

  (vlax-for x lays
    (setq modmad (vla-get-name x))
   (setq lss (ssget "X" (list (cons 0 "INSERT")(cons 66 1)(cons 410 modmad)))) ;all blocks with attributes in layout
   (setq a 0)
   (setq cnt 0)
   (repeat (sslength lss)
     (setq attlist (getattributes (vlax-ename->vla-object (ssname lss a))))
     (setq descatt (cdr (assoc "DESC" attlist))) ;description attribute -nil if not
     (if (/= descatt nil)
       (progn
         (setattributevalue (vlax-ename->vla-object (ssname lss a)) "INDEX" descatt)
        (setq cnt (+ 1 cnt))))


     (setq a (+ a 1)))

   (princ (strcat "\n" modmad " " (rtos cnt 2 0) " indeces set."))))
