;Used to update all references in a drawing. Scans through
;all blocks that contain a SOURCEREFID and DESTREFID and updates their references.
(defun crossref3 ()
  (if (setq ss (ssget "X" (list (cons 2 "BorderArchD"))))
    (setq jobcons (getattributevalue (vlax-ename->vla-object (ssname ss 0)) "LETTER_SECTION"))
    (setq jobcons ""))

  (setq ss (ssget "X" (list (cons 66 1))))

  (setq i 0)
  (repeat (sslength ss)
    (setq refstr "")
    (setq srefid (getattributevalue (vlax-ename->vla-object (ssname ss i)) "SOURCEREFID"))
    (if (/= srefid nil)
      (progn
       (setq sheet (cdr (assoc 410 (entget (ssname ss i)))))

       (foreach lay (layoutlist)
           (setq linestr "")
           (setq ss2 (ssget "X" (list (cons 66 1)(cons 410 lay))))
           (setq k 0)
         (repeat (sslength ss2)
           (setq drefid (getattributevalue (vlax-ename->vla-object (ssname ss2 k)) "DESTREFID"))
           (if (= drefid srefid)
             (progn
          ;Using location to calculate line number
               (setq point (cdr (assoc 10 (entget (ssname ss2 k)))))
               (if (< (nth 0 point) 14)
                (setq bline 45)
                (setq bline 90))

          ;line number calculation
               (setq line (+ 0 (fix (- bline (* 2 (- (nth 1 point) 0.25))))))
               (if (= linestr "")
                (setq del "")
                (setq del ","))

               (setq linestr (strcat linestr del (rtos line 2 0))) ;string containing line number

          ;doing the same for other block
               (setq point (cdr (assoc 10 (entget (ssname ss i)))))
               (if (< (nth 0 point) 14)
                (setq bline 45)
                (setq bline 90))

          ;(setq line (+ 1(fix (- bline (* 2 (nth 1 point))))))
               (setq line (+ 0 (fix (- bline (* 2 (- (nth 1 point) 0.25))))))
               (if (not (setq pref (getattributevalue (vlax-ename->vla-object (ssname ss2 k)) "PREREF")))
                (setq pref ""))

               (setq refstr2 (strcat pref "REF SHEET " sheet ", LINE " (rtos line 2 0)))
               (setattributevalue (vlax-ename->vla-object (ssname ss2 k)) "REF" refstr2)))


           (setq k (+ 1 k)))

         (if (/= linestr "")
          (progn
            (if (vl-string-search "," linestr)
              (setq refstr (strcat refstr "REF SHEET " lay ", LINES " (ordernums linestr)))
              (setq refstr (strcat refstr "REF SHEET " lay ", LINE " (ordernums linestr))))

            (setattributevalue (vlax-ename->vla-object (ssname ss i)) "REF" refstr)
            (setq refstr (strcat refstr "\n")))))



       (if (setq pref (getattributevalue (vlax-ename->vla-object (ssname ss i)) "PREREF"))
         (progn
          (setq psref (getattributevalue (vlax-ename->vla-object (ssname ss i)) "REF"))
          (setattributevalue (vlax-ename->vla-object (ssname ss i)) "REF" (strcat pref psref))))))




    (setq i (+ i 1))))



;Returns whether there is number in the string
(defun numbsinstring (str)
  ;; Removes the non-numeric characters from a string
  (vl-list->string
   (vl-remove-if-not 'num-char-p
                     (vl-string->list str))))
(defun num-char-p (char)
  ;; Does (chr num) represent a numeric character (0...9)?
 (< 46 char 58))


;This function sorts a string of numbers so these are displayed
;in order in a cross reference
(defun ordernums (str)
  (setq listnums (sparser str ","))
  (setq outstr "")
  (setq kk 0)
  (repeat (- (vl-list-length listnums) 1)
    (setq k 0)
    (repeat (- (vl-list-length listnums) 1)
      (if (> (atoi (nth k listnums))(atoi (nth (+ k 1) listnums)))
       (progn
         (setq temp (nth (+ k 1) listnums))
         (setq listnums (replace-element (+ k 1) (nth k listnums) listnums))
         (setq listnums (replace-element k temp listnums))))


      (setq k (+ 1 k))))


  (foreach num listnums
    (if (= outstr "")
      (setq outstr num)
      (setq outstr (strcat outstr ", " num)))))





(defun sparser (str delim / ptr lst)
 (while (setq ptr (vl-string-search delim str))
  (setq lst (cons (substr str 1 ptr) lst))
  (setq str (substr str (+ ptr 2))))

 (reverse (cons str lst)))


(defun replace-element (index newelement lst / tmp)
  (repeat index
    (setq tmp (cons (car lst) tmp)
     lst (cdr lst)))


  (append (reverse tmp) (list newelement) (cdr lst)))


;Function used to create reference bubbles
(defun c:bl ()
  (setq attr (getvar "attreq"))
  (setvar "attreq" 1)
  (command "LEADER" (getpoint) pause "" "" "n")
  (setq ss (ssget "L"))
  (setq elist (entget (ssname ss 0)))
  (setq i 0)
  (foreach ls elist
    (if (= (nth 0 ls) 10)
      (if (= i 0)
       (progn
         (setq i 2)
         (setq p1 (cdr ls)))

       (setq p2 (cdr ls)))))




  (setq diff (list (- (nth 0 p2)(nth 0 p1)) (- (nth 1 p2)(nth 1 p1))))
  (if (> (abs (nth 0 diff)) (abs (nth 1 diff)))
      (setq diff (list (/ (nth 0 diff) (abs (nth 0 diff))) 0))
      (setq diff (list 0 (/ (nth 1 diff) (abs (nth 1 diff))))))


  (while (/= (setq id (getstring "Set bubble number <0>: ")) "")
    (command "insert" "AIBUB" p2 "" "" "" id)
    (setq p2 (list (+ (nth 0 p2)(* (nth 0 diff) 0.4674)) (+ (nth 1 p2)(* (nth 1 diff) 0.4674)))))

  (setq elist (subst (cons 8 "BALLOON") (assoc 8 elist) elist))
  (entmod elist)
  (command "draworder" ss "" "back")
  (setvar "attreq" attr))
