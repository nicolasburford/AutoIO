
All of the lisp files in this folder are used to run AutoIO and load custom functions. Their individual functions are:

references - deals with crossreferences in a drawing. the bl bubble reference is found here as well.
plc5functions - functions relating to plc5 are found here. these import the rack and deal with bringing in the correct templates.
pfunctions - functions relating to point are found here. these import the rack and deal with bringing in the correct templates.
nfunctions - functions relating to the network switch are found here.
main - the main function called when pressing the button is found here. this function will then call the appropriate rack functions.
LoadAutoIO - this script is run when a drawing opens. it simply loads all of the necessary files into AutoCAD.
interconnect - functions relating to interconnect drawings can be found here
GetExcel - function found online to read an excel spreadsheet. this function is not as efficient as the custom vb function used but it is still useful to have.
functions - smaller functions used to support higher level files are found here.
ffunctions - functions relating to flex are found here. these import the rack and deal with bringing in the correct templates.
dialogboxes - functions relating to forms and dialogboxes are found here
clfunctions - functions relating to controllogix are found here. these import the rack and deal with bringing in the correct templates. NOTE: this file contains the main routine called by all other rack types to import descriptions and symbols.
cfunctions - functions relating to compact are found here. these import the rack and deal with bringing in the correct templates.
AutoIO Setup - functions relating to updates, versions, and setting up the tools can be found here
AutoIOTools - this file is not a lisp file and creates the custom buttons to launch all of the tools.

srxtext.vlx - function found online to run a find and replace on the entire drawing.