
;pplace - Builds the rack and text in sheet 9.
(defun newpplace (slots)

;Selecting sheet 9, autosnap off, initial point conditions

  (setvar 'osmode 0)
  (setq stpt (list 6.15 17.75))
  (setq topt stpt)
  (setq i 0)
  ;;Inserting processor and power
  (insblock "09" topt (findfile "newpstart.dwg"))

  (foreach slot  slots
    (if (/= i 1)
      (progn
       (if (AND (= 0 (rem (+ i 2) 12)) (/= i 0))
         (progn
           (insblock "09" topt (findfile "1734-EP24DCnbl.dwg"))
           (vla-explode iblk)
           (vla-delete iblk)
           (setq topt (list (+ 0.91 (nth 0 topt)) (nth 1 topt)))))


       (if (findfile (strcat slot "nbl.dwg"))
         (progn
           (insblock "09" topt (findfile (strcat slot "nbl.dwg")))
           (setq w (getwidth (vlax-vla-object->ename iblk)))
           (princ w)
           (vla-explode iblk)
           (vla-delete iblk))

         (alert (strcat slot " block not found.")))

      ;writing correct module numbers and sheets on modules
       (setq obj (ssget "X" (list (cons 410 "09"))))
       (setq modn (formatnum (- i 1)))
       (srxTEXT "Substring" "X1" modn obj)
       (setq shtn (rtos (+ i 8) 2 0))
       (srxTEXT "Substring" "X2" shtn obj)
       ;(setq w (getwidth (vla-object->vlax-ename iblk)))
       (setq topt (list (+ w (nth 0 topt)) (nth 1 topt)))
      )
    )
    (setq i (+ i 1))
  )
  (insblock "09" topt (findfile "newpend.dwg"))
  (setq topt stpt)
  (insblock "09" topt (findfile "newmp2bl.dwg"))
  (vla-explode iblk)
  (vla-delete iblk)

)


 (defun newpinputs ()

  (setq cnum "1734")
  (setq file "6174-255 Red Stag Rotorua IO List.xlsm")
  (setq data (getexceldata (findfile file) (strcat conso " IO")))
  (setq cnt (length data))

  (if (= (nth 0 data) "SheetFail")
    (progn
      (alert (strcat "Sheet " conso " IO not found."))
      (setq cnt 0)))

  (setq slots '())
  (setq i 0 k 0 icnt 0 isf 0 osf 0 ocnt 0 tcnt 0)
  (setq flag 1 pageint 9)

  (while (< i cnt)
    (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (getdata data i 0) "EMPTY"))
      (progn
       (setq slot (getdata data i 0))
       (setq slots (append slots (list slot)))

       (if (AND (> k 1)(/= slot "EMPTY"))
        (progn
            (if (AND (/= (substr (getdata data (+ i 1) 0) 1 4) cnum)(/= (getdata data (+ i 1) 0) "")(/= (getdata data (+ i 1) 0) "EMPTY")(/= (getdata data (+ i 1) 0) "ControlLogix.0")(not (vl-string-search "Analog" (getdata data i 1)))) ;if it has inputs
              (progn
                (setq slotnum (formatnum (- k 1)))
                ;Missing logic to decide when module already exists.
                ;(addlayout slot (+ k 8) slotnum)
                (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
                  (progn
                    (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
                      (setq pname (rtos (+ k 8) 2 0))
                      (addlayout slot (+ k 8) slotnum)
                    )
                  )
                  (addlayout slot (+ k 8) slotnum)
                )
                (setq iocnt 0)
                (inputsh)
              )
            )
          )
        )
       (setq k (+ 1 k))))
    (setq i (+ 1 i)
    )
  )
)








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
