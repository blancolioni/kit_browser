package Kit.UI is

   type Root_Kit_UI is abstract tagged private;

   function Choose_Database
     (With_UI : Root_Kit_UI)
     return String
      is abstract;

   procedure Show_Table
     (With_UI    : Root_Kit_UI;
      Table_Name : String)
     is abstract;

   procedure Start (UI   : Root_Kit_UI;
                    Path : String)
   is abstract;

   type Kit_UI is access constant Root_Kit_UI'Class;

private

   type Root_Kit_UI is abstract tagged null record;

end Kit.UI;
