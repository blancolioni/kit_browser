private with Ada.Strings.Unbounded;
private with Gtk.Tree_View;
private with Kit.Db.Tables.Scanner;

with Gtk.Widget;

package Kit.UI.Gtk_UI.Table_Book is

   type Root_Table_Display is tagged private;

   function Table_Name (Display : Root_Table_Display'Class)
                        return String;

   function Widget (Display : Root_Table_Display'Class)
                    return Gtk.Widget.Gtk_Widget;

   procedure Update (Display  : in out Root_Table_Display'Class;
                     Max_Rows : Positive;
                     Changed  : out Boolean);

   type Table_Display is access all Root_Table_Display'Class;

   function New_Table_Display
     (Table_Name : String)
      return Table_Display;

private

   type Root_Table_Display is tagged
      record
         Widget     : Gtk.Tree_View.Gtk_Tree_View;
         Table_Name : Ada.Strings.Unbounded.Unbounded_String;
         Key_Name   : Ada.Strings.Unbounded.Unbounded_String;
         Scan       : Kit.Db.Tables.Scanner.Table_Scanner;
      end record;

end Kit.UI.Gtk_UI.Table_Book;
