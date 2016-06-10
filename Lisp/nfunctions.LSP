
;ninputs - inserts all entries to separate layouts if necessary
(defun ninputs ()
  (setq data (getexcel file "Network Switch" "B3"))
  (setq enclnum (atoi (getdata data 2 1)))
  (setq data (getexcel file "Network Switch" (strcat "G" (rtos (+ (* 35 enclnum) 4) 2 0))))

  (setvar 'osmode 0) ;disables osnap

  (setq corn '(27.8125 3.9975)) ;corner of usnr block used as insertion point
  (setq i 0)

  (setq to1 '(0.326658 20.787)) ;insertion points for icons
  (setq to2 (list 3.8259 (- 15.7423 2.0546)))
  (setq to3 '(3.82586 11.3173))
  (setq to4 (list 0.326658 (- 6.27334 2.0546)))

  (setq p 0)
  (setq k 0)
  (setq flag 0)
  (while (= flag 0)
    (if (= (getdata data k 0) "Ethernet Port")
      (progn
	(setq p (+ p 10))
  (setq slotnum (formatnum (+ p 10)))
	(addlayout "01a" p slotnum)

	(setq k (+ k 1))
	(setq i 1)
	(setq to1 '(0.326658 20.787)) ;insertion points for icons
	(setq to2 (list 3.8259 (- 15.7423 2.0546)))
	(setq to3 '(3.82586 11.3173))
	(setq to4 (list 0.326658 (- 6.27334 2.0546)))
	(while (< i 27)
	  (if (/= (getdata data k 2) "")
	    (progn
	      (insblock (rtos p 2 0) corn (findfile (strcat "ethblock" (rtos i 2 0) ".dwg")))
	      ;(command "._insert" (strcat "ethblock" (rtos i 2 0)) corn "" "" "") ;inserting tags
	      (setq blockRef (explodesel iblk "NETWORK_DETAIL"))
	      (setattributevalue blockRef "NAME1" (getdata data k 1))
	      (setattributevalue blockRef "LOCATION" (getdata data k 2))
	      (setattributevalue blockRef "MODULE_NAME" (getdata data k 3))
	      (setattributevalue blockRef "REFDWG" (getdata data k 4))
	      (setattributevalue blockRef "IPADDRESS" (getdata data k 5))

	    ;inserting icons
	      (if (OR (< i 5)(> i 22)) ;middle two rows
		(progn
		  (if (= (rem i 2) 1) ;row2
		    (progn
		      (insblock (rtos p 2 0) to2 (findfile (strcat (getdata data k 6) "nb" ".dwg")))
		    )
		    (progn ;else row3
		      (insblock (rtos p 2 0) to3 (findfile (strcat (getdata data k 6) "nb" ".dwg")))
		    )
		  )
	        )
	        (progn ;else (top and bottom rows)
		  (if (= (rem i 2) 1) ;row1
		    (progn
		      (insblock (rtos p 2 0) to1 (findfile (strcat (getdata data k 6) "nb" ".dwg")))
		    )
		    (progn ;else row 4
		      (insblock (rtos p 2 0) to4 (findfile (strcat (getdata data k 6) "nb" ".dwg")))
		    )
		  )
	        )
	      )
	    )
	  )
	;regardless of whether empty or not, update insertion points
	  (if (OR (< i 5)(> i 22))
	    (progn
	      (if (= (rem i 2) 1)
	        (setq to2 (list (- (nth 0 to2) 3.4936) (nth 1 to2)))
	        (setq to3 (list (- (nth 0 to3) 3.4936) (nth 1 to3)))
	      )
	      (if (= i 3) ;shift row2 right
	        (setq to2 (list (+ (nth 0 to2) (+ 29.8509 3.4936)) (nth 1 to2)))
	      )
	      (if (= i 4) ;shift row3 right
	        (setq to3 (list (+ (nth 0 to3) (+ 29.8509 3.4936)) (nth 1 to3)))
	      )
	    )
	    (progn
	      (if (= (rem i 2) 1)
	        (setq to1 (list (+ (nth 0 to1) 3.4936) (nth 1 to1)))
	        (setq to4 (list (+ (nth 0 to4) 3.4936) (nth 1 to4)))
	      )
	    )
	  )
	  (setq i (+ i 1))
	  (setq k (+ k 1))
	);while
	(setq k (+ k 1))
	(setq ss (ssname (ssget "X" (list (cons 2 "IPs")(cons 410 (rtos p 2 0)))) 0))
	(setattributevalue (vlax-ename->vla-object ss) "IP1" (getdata data k 1))
	(setattributevalue (vlax-ename->vla-object ss) "GW" (getdata data k 3))
	(setattributevalue (vlax-ename->vla-object ss) "SN" (getdata data k 5))

    	(setq i 1)
    	(repeat 5
    	  (setq i (+ i 1))
    	  (setq k (+ k 1))
	  (setattributevalue (vlax-ename->vla-object ss) (strcat "IP" (rtos i 2 0)) (getdata data k 1))
    	)
      );eth progn
    );if
    (setq k (+ k 1))

    (if (> k (- (length data) 1))
      (setq flag 1)
    )
  )
  (tab_sort)
  (princ)
)


(defun icninput()

  (setq data (getexceldata (findfile file) (strcat "Network")))
  (setq cnt (length data))

  ;if layout doesnt exist add it
  (if (member "10" (layoutlist))
    (setq pname "10")
    (addlayout "ProCurve" 10 "00"))


  (setq i 2)
  (setq pgcnt 0)
  (setq ss (ssget "X" (list (cons 410 pname) (cons 66 1))))
  ;go through rows
  (while (< i cnt)
    (setq k 0)
    (repeat (sslength ss)
      (setq blockRef (vlax-ename->vla-object (ssname ss k)))
      (setq attlist (getattributes blockRef))
      (setq index (cdr (assoc "ITEM" attlist)))
      ;find the block with the right index and set all values
      ;(setq blcnum (formatnum (- i 1)))
      (if (= index (formatnum (- (- i 1) (* pgcnt 26))))
       (progn
	      (setattributevalue blockRef "NAME1" (getdata data i 0))
	      (setattributevalue blockRef "LOCATION" (getdata data i 1))
	      (setattributevalue blockRef "MODULE_NAME" (getdata data i 2))
	      (setattributevalue blockRef "REFDWG" (getdata data i 3))
	      (setattributevalue blockRef "IPADDRESS" (getdata data i 4))
        (if (findfile (strcat (getdata data i 5) "nb.dwg"))
          (progn
            (setq topt (cdr (assoc 10 (entget (ssname ss k)))))
            (insblock (formatnum (+ 10 pgcnt)) topt (findfile (strcat (getdata data i 5) "nb.dwg")))
          )
        )
      )
    )


      (setq k (+ k 1)))

    (setq i (+ i 1))
    (if (and (> (- i 1) (* (+ 1 pgcnt) 26))(< i cnt))
      (progn
       (setq pgcnt (+ pgcnt 1))
       (if (member (rtos (+ 10 pgcnt) 2 0) (layoutlist))
         (setq pname (rtos (+ 10 pgcnt) 2 0))
         (progn
           (setq slotnum (formatnum (+ 10 pgcnt)))
           (addlayout "ProCurve" (+ 10 pgcnt) "00"))
         )
       (setq ss (ssget "X" (list (cons 410 pname) (cons 66 1))))
       )
    )
  )
)
(defun deltxt ()

  (setq i 0)
  (setq ss (ssget "X" (list (cons 410 "ProCurve") (cons 66 1))))
  (repeat (sslength ss)
    (setq blockRef (vlax-ename->vla-object (ssname ss i)))
    (setq attlist (getattributes blockRef))
    (setq index (cdr (assoc "ITEM" attlist)))
    (if (/= index nil)
      (progn
	      (setattributevalue blockRef "NAME1" "")
	      (setattributevalue blockRef "LOCATION" "")
	      (setattributevalue blockRef "MODULE_NAME" "")
	      (setattributevalue blockRef "REFDWG" "-")
	      (setattributevalue blockRef "IPADDRESS" "-")
      )
    )
    (setq i (+ i 1))))
