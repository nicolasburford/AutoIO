;;This file contains the main commands for AutoIO.

;Main is the highest level function that is called when the user
;presses the AutoIO button.
(defun c:main ()
  (TEST_DCL2)             ;dialog box
  ;(setq userclick T)
  ;(setq SIZ 0)
  ;(setq proj "YYYY")
  ;(setq name2 "055")
  ;(setq conso "E1M1")
  ;(setq file "Master Test.xls")
  (setq laystate (getvar "clayer"))
  (setvar "clayer" "0")
  (setq snapstate (getvar 'osmode))
  (if (and (= userclick T)(findfile file))     ;if accept is pressed
    (progn
      (cond
       ((= SIZ 0)        ;ControlLogix
        (clmain))

       ((= SIZ 2)        ;Point IO
        (pmain))

       ((= SIZ 1)        ;Flex IO
        (fmain))

       ((= SIZ 3)        ;Compact
        (cmain))

       ((= SIZ 4)        ;Compact
        (plc5main))

       ((= SIZ 5)        ;Network
        (nsmain))

       ((= SIZ 6)        ;Interconnect
        (intcntmain)))


      (setvar 'osmode snapstate)
      (srxTEXT "Substring" "1234" conso "All")
      (tab_sort)
      (zext)
     (crossref3)
     (srxTEXT "Substring" "YYYY" proj "All")  ;Finding and Replacing
     (srxTEXT "Substring" "055" name2 "All")
     (srxTEXT "Substring" "1234" conso "All")
     (princ "AutoIO successful.")
     (alert "Auto IO Complete.\n"))

    (progn
      (if (= userclick F)
       (princ "AutoIO cancelled.")
       (alert "Could not find file."))))



  (setvar "clayer" laystate))


;Main function for populating Compact template.
(defun cmain ()
  (setq tfile "1769-Compact_AutoIO_Template.dwg") ;TEMPLATE FILE LOCATION

 (cinputs)
 (if (= rackdrop 1)
   (cplace slots)))



;Main function for populating Flex template.
(defun fmain ()
  (setq tfile "1794_Flex_AutoIO_Template.dwg") ;TEMPLATE FILE LOCATION

 (finputs)
 (if (= rackdrop 1)
   (fplace slots)))



;Main function for populating Point IO template.
(defun pmain ()
  (setq tfile "1734_Point_AutoIO_Template.dwg") ;TEMPLATE FILE LOCATION

 (pinputs)
 (if (= rackdrop 1)
   (pplace slots)))


(defun plc5main ()
  (setq tfile "1771_PLC5_AutoIO_Template.dwg") ;TEMPLATE FILE LOCATION
 (plinputs))


;Main function for populating ControlLogix template.
(defun clmain ()
  (setq tfile "1756_Logix_AutoIO_Template.dwg") ;TEMPLATE FILE LOCATION

 (clinput)

 (if (= rackdrop 1)
   (placesh slots))

 ;(place slots)                   ;placing correct modules on rack
 ;(command "ctab" "09")
 ;(command "ELECAUTOBOM")

 (princ))


;Main function for populating Network template.
(defun nsmain (/ to1 to2 to3 to4 corn obj)
  (setq tfile "Network_AutoIO_Template.dwg") ;TEMPLATE FILE LOCATION

 (ninputs)          ;populating everything

 (prelim)          ;Adding preliminary stamps
 (command "ctab" "10"))


(defun intcntmain ()
  (setq tfile "Interconnect_AutoIO_Template.dwg")
  (icinput))


(defun zext()
  (setq temp (getvar "ctab"))
  (foreach lay (layoutlist)
    (command "ctab" lay)
    (command "ZOOM" "EXTENTS"))

  (command "ctab" temp))
