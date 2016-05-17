

//Main dialog box
test_dcl2 : dialog {
 
	label = "Console Information";

	:column {
	
	: edit_box
	{
	label = "Project #:";
	mnemonic = "";
	key = "proj";
	alignment = centered;
	edit_limit = 4;
	edit_width = 4;
	}

	: edit_box
	{
	label = "Other Number:";
	mnemonic = "";
	key = "name2";
	alignment = centered;
	edit_limit = 3;
	edit_width = 3;
	}

	: edit_box
	{
	label = "Console Name:";
	mnemonic = "";
	key = "conso";
	alignment = centered;
	edit_width = 4;
	}
	
	:row {
     	: text {				//define text
     	label = "Console Type:";
     	}
     	: toggle { 
                  key = "rackdrop"; 
                   label = "Add Rack"; 
                }
                
	: popup_list {                          //define popup list
        key = "selections";			//give it a name
        value = "0" ;				//initial value
	fixed_width = true;
	alignment = right;
        }
        }
	:row{
	
	: edit_box
	{
	label = "I/O File:";
	mnemonic = "";
	key = "file";
	alignment = centered;
	edit_width = 30;
	}

	: button {
	label = "Browse...";
	key = "browse";
	mnemonic = "";
	alignment = centered;
	width = 17;
	}

	}

	ok_cancel;
 
	: errtile
	{
	width = 34;
	}

	}
}

//stamp dialog
stamps : dialog {				//dialog name
      label = "Select Stamp" ;		//give it a label
 
       :boxed_radio_column {			//define radio column
       label = "Stamps" ;				//give it a label
 
        : radio_button {			//define radion button
     	  key = "rb1" ;				//give it a name
     	  label = "Approved" ;		//give it a label
     	  value = "1" ;				//switch it on
        }					//end definition
 
     	: radio_button {			//define radio button
     	  key = "rb2" ;				//give it a name
     	  label = "As Built" ;		//give it a label
     	}					//end definition
 
     	: radio_button {			//define radio button
     	  key = "rb3" ;				//give it a name
     	  label = "For Approval" ;	//give it a label
     	  }					//end definition
 
     	: radio_button {			//define radio button
     	  key = "rb4" ;				//give it a name
     	  label = "For Info Only" ;		//give it a label
     	}					//end definition
 
     	: radio_button {			//define radio button
     	  key = "rb5" ;				//give it a name
     	  label = "Obsolete" ;	//give it a label
     	  }					//end definition
 
     	: radio_button {			//define radion button
     	  key = "rb6" ;				//give it a name
     	  label = "Preliminary" ;	//give it a label
     	}					//end definition
 
        }					//end radio column
 
     ok_cancel ;				//predifined OK/Cancel						//end row
					
     }//end dialog

     
open : dialog {
 
	label = "USNR Electrical Files";

	:column {
	
	: edit_box
	{
	label = "Item:";
	mnemonic = "";
	key = "proj";
	alignment = centered;
	edit_limit = 20;
	edit_width = 20;
	}


	ok_cancel;
 
	: errtile
	{
	width = 34;
	}

	}
}

//revision dialog box
rev : dialog {
 
	label = "Update Drawing Revision";

	:column {
	
	: edit_box
	{
	label = "New Revision:";
	mnemonic = "";
	key = "revv";
	alignment = centered;
	edit_limit = 5;
	edit_width = 5;
	}


	ok_cancel;
 
	: errtile
	{
	width = 34;
	}

	}
}