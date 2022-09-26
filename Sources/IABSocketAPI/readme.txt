//**************************************************************************
//
//    IABSocketAPI - for use with InteractiveBrokers TradeWorkStation(TWS)
//                   direct order placement API
//
//    by Ross Hemingway - iabsocketapi@hhssoftware.com
//
//    https://www.hhssoftware.com/iabsocketapi/
//
//
//**************************************************************************


1.  Files located in the API_all_env folder:

       Move / copy the

         IABSocketAPI.pas
   	 IABSocketAPI_const.pas
	 IABSocketAPI_tcpclient.pas
         IABSocketAPI.dcr


       files to your library or add-on folder.  Install these into the library and rebuild.
       The component will appear on the Samples page.  Depending on your version of Delphi,
       it may also ask to include the inet.dcp file.  Accept this and include.


     Decide right now, do you want to trade bitcoins?
   
       Then you need to add and include the BigDecimals library.  Read the BigDecimal.txt file
       for procedures to do this step now.


     For those not familiar with installing components into the DCL ....

        In XE, from the menu, select Component - > Component Install.
        Add the IABsocketAPI.pas files above to the package.
        The default package is dclusr.dpk, but any name will do, or a new one.
        Build the package.
        Install the package (right click the Project Manager file tree -> install).
        Save and close the package.
      
     Now you should see the TIABSocketAPI component on the Samples tab of the VCL, ready to drop on a form.

      
      

2.  Extract the demo project, Located in the Delphi-BCB folder:

      Compile the demo app IABSocket project.  If you chose to use BigDecimal, then read the file
        BigDecimal.txt file for procedures to do this step now.


3.  Look through the demo app provided for samples and notes.  Also the main 
      help file is online at the link above.  Look there for ammendments and updates.



Happy trading

Ross Hemingway
