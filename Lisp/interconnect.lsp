
(defun icinput()

  (setq data (getexceldata (findfile file) (strcat "Interconnect - Electrical")))
  (setq cnt (length data))

  ;if layout doesnt exist add it
  (if (member "10" (layoutlist))
    (setq pname "10")
    (addlayout "Main" 10 "00"))


  (setq i 2)
  (setq pgcnt 0)
  (setq ss (ssget "X" (list (cons 410 pname) (cons 2 "BLOCK WIRING LABEL3"))))
  ;go through rows
  (while (< i cnt)
    (setq k 0)
    (repeat (sslength ss)
      (setq blockRef (vlax-ename->vla-object (ssname ss k)))
      (setq index (getattributevalue blockRef "INDEX"))
      ;find the block with the right index and set all values
      (if (= index (rtos (- (- i 1) (* pgcnt 34)) 2 0))
       (progn
         (setattributevalue blockRef "LOCATION" (getdata data i 1))
         (setattributevalue blockRef "NAME1" (getdata data i 0))
         (setattributevalue blockRef "REFERENCE_DRAWING" (strcat "REF DWG " (getdata data i 2)))
         (setattributevalue blockRef "1_CABLE_TYPE" (getdata data i 3))
         (setattributevalue blockRef "1_SUPPLIED_BY" (getdata data i 4))
         (setattributevalue blockRef "2_CABLE_TYPE" (getdata data i 5))
         (setattributevalue blockRef "2_SUPPLIED_BY" (getdata data i 6))
         (setattributevalue blockRef "3_CABLE_TYPE" (getdata data i 7))
         (setattributevalue blockRef "3_SUPPLIED_BY" (getdata data i 8))))


      (setq k (+ k 1)))

    (setq i (+ i 1))
    (if (and (> (- i 1) (* (+ 1 pgcnt) 34))(< i cnt))
      (progn
       (setq pgcnt (+ pgcnt 1))
       (if (member (rtos (+ 10 pgcnt) 2 0) (layoutlist))
         (setq pname (rtos (+ 10 pgcnt) 2 0))
         (setq slotnum (formatnum (+ 10 pgcnt)))
         (addlayout "Main" (+ 10 pgcnt) slotnum))

       (setq ss (ssget "X" (list (cons 410 pname) (cons 2 "BLOCK WIRING LABEL3"))))))))





(defun deltxt ()

  (setq i 0)
  (setq ss (ssget "X" (list (cons 2 "BLOCK WIRING LABEL3"))))
  (repeat (sslength ss)
    (setq blockRef (vlax-ename->vla-object (ssname ss i)))
    (setattributevalue blockRef "LOCATION" "")
    (setattributevalue blockRef "NAME1" "")
    (setattributevalue blockRef "REFERENCE_DRAWING" "")
    (setattributevalue blockRef "1_CABLE_TYPE" "-")
    (setattributevalue blockRef "1_SUPPLIED_BY" "-")
    (setattributevalue blockRef "2_CABLE_TYPE" "-")
    (setattributevalue blockRef "2_SUPPLIED_BY" "-")
    (setattributevalue blockRef "3_CABLE_TYPE" "-")
    (setattributevalue blockRef "3_SUPPLIED_BY" "-")
    (setq i (+ i 1))))
