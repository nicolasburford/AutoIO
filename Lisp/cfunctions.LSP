
;;cplace - Builds the rack and text in sheet 9.
(defun cplace (slots)

  ;;Selecting sheet 9, autosnap off, initial point conditions
  (setvar 'osmode 0)
  (setq topt (list 5.01112 16.0881))


  ;;Inserting processor
  (if (findfile (strcat (nth 2 slots) "wh.dwg"))
    (progn
      (insblock	"09"
		topt
		(findfile (strcat (nth 2 slots) "wh.dwg"))
      )
      (vla-explode iblk)
      (vla-delete iblk)
    )
    (alert (strcat (nth 2 slots) " block not found."))
  )

  (setq obj (ssget "X" (list (cons 410 "09"))))
  (srxTEXT "Substring" "XX" "01" obj)
  (srxTEXT "Substring" "X1" "10" obj)

  ;;Inserting power source
  (setq topt (list (+ 2.5840 (nth 0 topt)) (nth 1 topt)))

  (if (findfile (strcat (nth 0 slots) "wh.dwg"))
    (progn
      (insblock	"09"
		topt
		(findfile (strcat (nth 0 slots) "wh.dwg"))
      )
      (vla-explode iblk)
      (vla-delete iblk)
    )
    (alert (strcat (nth 0 slots) " block not found."))
  )

  ;;Inserting modules
  (setq i 0)
  (foreach slot	slots
    (if	(> i 2)
      (progn
	(if (findfile (strcat slot "wh.dwg"))
	  (progn
	    (insblock "09" topt (findfile (strcat slot "wh.dwg")))
	    (vla-explode iblk)
	    (vla-delete iblk)
	  )
	  (alert (strcat slot " block not found."))
	)

	(setq obj (ssget "X" (list (cons 410 "09"))))
	(srxTEXT "Substring" "XX" (formatnum (- i 2)) obj)
	(srxTEXT "Substring" "X1" (rtos (+ i 8) 2 0) obj)

	(setq topt (list (+ 1.2988 (nth 0 topt)) (nth 1 topt)))
      )
    )
    (setq i (+ i 1))
  )
  (insblock "09" topt (findfile "endwh.dwg"))
)

(defun cinputs ()

  (setq cnum "1769")

  ;Reading data from excel
  (setq data (getexceldata (findfile file) (strcat conso " IO")))
  (setq cnt (length data))
  (if (= (nth 0 data) "SheetFail")
    (progn
      (alert (strcat "Sheet " conso " IO not found."))
      (setq cnt 0)
    )
  )
  
  (setq slots '())

  (setq i 0)
  (setq k 0)
  (setq flag 1)

  ;Running down all rows
  (while (< i cnt)

    ;Find a slot
    (if (OR (= (substr (getdata data i 0) 1 4) cnum)(= (getdata data i 0) "EMPTY"))
      (progn

	;Add slot to list
	(setq slot (getdata data i 0))
	(if (= (substr slot (strlen slot) 1) " ")
	  (setq slot (substr slot 1 (- (strlen slot) 1)))
	)
      (setq slots (append slots (list slot)))

      (if (AND (> k 1)(/= slot "EMPTY"))
	(progn
	  ;If slot has inputs
	  (if (AND (/= (substr (getdata data (+ i 1) 0) 1 4) cnum)(/= (getdata data (+ i 1) 0) "")(/= (getdata data (+ i 1) 0) "EMPTY")(/= (getdata data (+ i 1) 0) "ControlLogix.0")(not (vl-string-search "Analog" (getdata data i 1)))) ;if it has inputs
	    (progn
	      ;If a sheet exists on given page. If its the same module keep it if its different
	      ;then add correct one
	      (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
		(progn
		  (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
		    (setq pname (rtos (+ k 8) 2 0))
		    (addlayout slot (+ k 8))
		  )
		)
		(addlayout slot (+ k 8))
	      )
	      ;Update IO
	      (setq iocnt 0)
	      (inputsh)
	    )
	    (progn
	      ;If it doesnt have inputs then do the same but never update inputs
	      (if (setq ss (ssget "X" (list (cons 410 (rtos (+ k 8) 2 0))(cons 2 "MODNAME"))))
		(progn
		  (if (= (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "MODNAME") slot)
		    (setq pname (rtos (+ k 8) 2 0))
		    (addlayout slot (+ k 8))
		  )
		)
		(addlayout slot (+ k 8))
	      )
	    )
	  )
	)
      )
      (setq k (+ 1 k))
      )
    )
    (setq i (+ 1 i))
  )

)
