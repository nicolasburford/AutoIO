
;pplace - Builds the rack and text in sheet 9.
(defun pplace (slots)

;Selecting sheet 9, autosnap off, initial point conditions

  (setvar 'osmode 0)
  (setq topt (list 6.75 15.0))

  (setq i -2)
  ;;Inserting processor and power
  (insblock "09" topt (findfile "Ptstart.dwg"))
  (insblock "09" topt (findfile "MP2bl.dwg"))
  (vla-explode iblk)
  (vla-delete iblk)

  (foreach slot  slots
    (if  (> i -1)
      (progn
  ;additional power module every 12
       (if (AND (= 0 (rem i 12)) (/= i 0))
         (progn
           (insblock "09" topt (findfile "EP3bl.dwg"))
           (vla-explode iblk)
           (vla-delete iblk)
           (setq topt (list (+ 0.91 (nth 0 topt)) (nth 1 topt)))))


       (if (findfile (strcat slot "bl.dwg"))
         (progn
           (insblock "09" topt (findfile (strcat slot "bl.dwg")))
           (vla-explode iblk)
           (vla-delete iblk))

         (alert (strcat slot " block not found.")))


  ;writing correct module numbers and sheets on modules
       (setq obj (ssget "X" (list (cons 410 "09"))))
       (if (< i 9)
         (setq modn (strcat "0" (rtos (+ i 1) 2 0)))
         (setq modn (rtos (+ i 1) 2 0)))

       (srxTEXT "Substring" "X1" modn obj)
       (if (< i icnt)
         (setq shtn (rtos (+ (/ i 4) 10) 2 0))
         (setq shtn (rtos (+ (+ (/ icnt 4) (/ (- i icnt) 4)) 11) 2 0)))

       (srxTEXT "Substring" "X2" shtn obj)

       (setq topt (list (+ 0.47 (nth 0 topt)) (nth 1 topt)))))


    (setq i (+ i 1)))

  (insblock "09" topt (findfile "Ptend.dwg")))


(defun pinputs()

  (setq cnum "1734")
  (setq pt1 (list 15.3125 13.25 0.0))
  (setq pt2 (list 15.3125 23.3125 0.0))
  (setq pt3 (list 27.1875 4.0 0.0))
  (setq pt4 (list 0.3125 13.25 0.0))

  (setq data (getexceldata (findfile file) (strcat conso " IO")))
  (setq cnt (length data))
  (if (= (nth 0 data) "SheetFail")
    (progn
      (alert (strcat "Sheet " conso " IO not found."))
      (setq cnt 0)))



  (setq slots '())

  (setq i 0)
  (setq k 0)
  (setq icnt 0)
  (setq isf 0)
  (setq osf 0)
  (setq ocnt 0)
  (setq tcnt 0)
  (setq flag 1)
  (setq pageint 9)
  (while (< i cnt)
    (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (getdata data i 0) "EMPTY"))
      (progn
       (setq slot (getdata data i 0))
       (setq slots (append slots (list slot)))

      ;check if sheet exists and if correct module
      ;if sheet doesnt exist create, if not correct module, delete
      ;if correct module check if it has input data if it does
      ;replace with updated template version. if not leave untouched
       (if (AND (> k 1)(/= slot "EMPTY"))
        (progn
          (if (AND (/= (substr (getdata data (+ i 1) 0) 1 4) cnum)(/= (getdata data (+ i 1) 0) "")(/= (getdata data (+ i 1) 0) "EMPTY")(/= (getdata data (+ i 1) 0) "ControlLogix.0")(not (vl-string-search "Analog" (getdata data i 1)))) ;if it has inputs
            (progn

              (setq slotnum (formatnum (+ tcnt 1)))
        ;If module is an input
              (if (= (substr (getdata data i 0) 6 1) "I")
               (progn
                 (if (= (substr (getdata data i 0) 9 1) "S")  ;Check whether it is safety
                   (progn
                     (setq modnum (rem isf 2))
                     (if (= modnum 0)
                      (progn
                        (setq pageint (+ 1 pageint))
                        (setq pagenum (rtos pageint 2 0))
                        (if (setq ss (ssget "X" (list (cons 410 pagenum)(cons 2 "MODNAME"))))
                          (progn
                            (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") "SINPUTS")
                             (setq pname (rtos pageint 2 0))
                             (addlayout "SINPUTS" pageint "00")))


                          (addlayout "SINPUTS" pageint "00"))

                        (setq iocnt 0)))


                     (setq isf (+ 1 isf))
                     (setq tcnt (+ 1 tcnt)))

                   (progn
                     (setq modnum (rem icnt 4))
                     (if (= modnum 0)
                      (progn
                        (setq pageint (+ 1 pageint))
                        (setq pagenum (rtos pageint 2 0))
                        (if (setq ss (ssget "X" (list (cons 410 pagenum)(cons 2 "MODNAME"))))
                          (progn
                            (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") "INPUTS")
                             (setq pname (rtos pageint 2 0))
                             (addlayout "INPUTS" pageint "00")))


                          (addlayout "INPUTS" pageint "00"))

                        (setq obj (ssget "X" (list (cons 410 pagenum))))
                        (if (= (substr conso 3 1) "C")
                          (srxTEXT "Substring" "24V1A-2" "24V1A-3" obj))

                        (setq iocnt 0)))


                     (setq icnt (+ icnt 1))
                     (setq tcnt (+ tcnt 1))))))




              (if (= (substr (getdata data i 0) 6 1) "O")
               (progn
                 (if (= (substr (getdata data i 0) 9 1) "S")  ;Check whether it is safety
                   (progn
                     (setq modnum (rem osf 2))
                     (if (= modnum 0)
                      (progn
                        (setq pageint (+ 1 pageint))
                        (setq pagenum (rtos pageint 2 0))
                        (if (setq ss (ssget "X" (list (cons 410 pagenum)(cons 2 "MODNAME"))))
                          (progn
                            (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") "SOUTPUTS")
                             (setq pname (rtos pageint 2 0))
                             (addlayout "SOUTPUTS" pageint "00")))


                          (addlayout "SOUTPUTS" pageint "00"))

                        (setq iocnt 0)))


                     (setq osf (+ 1 osf))
                     (setq tcnt (+ 1 tcnt)))

                   (progn
                     (setq modnum (rem ocnt 4))
                     (if (= modnum 0)
                      (progn
                        (setq pageint (+ 1 pageint))
                        (setq pagenum (rtos pageint 2 0))
                        (if (setq ss (ssget "X" (list (cons 410 pagenum)(cons 2 "MODNAME"))))
                          (progn
                            (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") "OUTPUTS")
                             (setq pname (rtos pageint 2 0))
                             (addlayout "OUTPUTS" pageint "00")))


                          (addlayout "OUTPUTS" pageint "00"))

                        (setq obj (ssget "X" (list (cons 410 pagenum))))
                        (if (= (substr conso 3 1) "C")
                          (srxTEXT "Substring" "24V2A" "24V1C" obj))

                        (setq iocnt 0)))


                     (setq ocnt (+ ocnt 1))
                     (setq tcnt (+ 1 tcnt))))))




              (if (or (and (> ocnt 0)(= (+ isf osf) 1))(and (= (+ (+ ocnt isf) osf) 1)(> icnt 0)))
                (progn
                 (setq prevpg (formatnum (- pageint 1)))
                 (command "ctab" prevpg)
                 (command "zoom" "extents")

                 (if (= (+ (+ ocnt isf) osf) 1)
                   (setq remy (rem icnt 4))
                   (setq remy (rem ocnt 4)))

                 (cond
                    ((= remy 3)
                     (command "erase" (ssget "W" pt1 pt3) ""))

                    ((= remy 2)
                     (command "erase" (ssget "W" pt2 pt3) ""))

                    ((= remy 1)
                     (command "erase" (ssget "W" pt2 pt3)(ssget "W" pt4 pt3) "")))



                 (setq obj2 (ssget "X" (list (cons 410 prevpg))))
                 (srxTEXT "Substring" "X4" (formatnum tcnt) obj2)))


              (cond
               ((= modnum 0)
                (srxTEXT "Substring" "X1" slotnum obj))

               ((= modnum 1)
                (srxTEXT "Substring" "X2" slotnum obj))

               ((= modnum 2)
                (srxTEXT "Substring" "X3" slotnum obj))

               ((= modnum 3)
                (srxTEXT "Substring" "X4" slotnum obj)))


        ;(setq tcnt (+ icnt ocnt))

              (inputsh)))))




       (setq k (+ 1 k))))


    (setq i (+ 1 i)))


  (if (or (and (> ocnt 0)(= osf 0))(and (= (+ (+ ocnt isf) osf) 0)(> icnt 0)))
    (progn
     (command "ctab" (nth (- (vl-list-length (layoutlist)) 1) (layoutlist)));last page
     (command "zoom" "extents")
     (if (= (+ ocnt isf) 0)
       (setq remy (rem icnt 4))
       (setq remy (rem ocnt 4)))


     (cond
        ((= remy 3)
         (command "erase" (ssget "W" pt1 pt3) ""))

        ((= remy 2)
         (command "erase" (ssget "W" pt2 pt3) ""))

        ((= remy 1)
         (command "erase" (ssget "W" pt2 pt3)(ssget "W" pt4 pt3) "")))



     (setq obj2 (ssget "X" (list (cons 410 pname))))
     (srxTEXT "Substring" "X4" slotnum obj2)))


  (if (= (substr conso 3 1) "C")
    (progn
      (setq ss (ssget "X" (list (cons 2 "Fuse"))))
      (setq i 0)
      (repeat (sslength ss)
       (vla-delete (vlax-ename->vla-object (ssname ss i)))
       (setq i (+ i 1)))

      (setq ss (ssget "X" (list (cons 2 "TBlock"))))
      (setq i 0)
      (repeat (sslength ss)
       (vla-delete (vlax-ename->vla-object (ssname ss i)))
       (setq i (+ i 1))))))







(defun powbl ()
  (if (and (= (substr (getdata data (+ i -1) 7) 1 3) "IPR")(< (+ i 0) cnt))
    (progn
     (if (/= (substr (getdata data (+ i 0) 7) 1 3) "IPR")
       (setq pwflg 1)))


    (progn
      (if (= cnt (+ 0 i))
        (setq pwflg 1))))



  (if (= pwflg 1)
    (progn
      (insblock pname point (findfile "IPRPW.dwg"))
      (vla-explode iblk)
      (vla-delete iblk))))
