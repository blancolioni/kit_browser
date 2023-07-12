with Glib;

with Gtk.Cell_Renderer_Text;
with Gtk.Handlers;
with Gtk.List_Store;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;

with Kit.Handles.Kit_Record;

package body Kit.UI.Gtk_UI.Table_Lists is

   package Tree_View_Callback is
     new Gtk.Handlers.User_Callback
       (Gtk.Tree_View.Gtk_Tree_View_Record,
        Kit_UI);

   procedure Select_Table_Handler
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      UI     : Kit_UI);

   ---------------------
   -- Fill_Table_List --
   ---------------------

   procedure Fill_Table_List
     (UI        : not null access constant Root_Kit_UI'Class;
      View      : Gtk.Tree_View.Gtk_Tree_View)
   is
      Store : Gtk.List_Store.Gtk_List_Store;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Text_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Num         : Glib.Gint;
      pragma Warnings (Off, Num);

   begin
      Gtk.List_Store.Gtk_New
        (Store,
         (1 => Glib.GType_String));

      for Table of Kit.Handles.Kit_Record.Scan_By_Name loop
         Store.Append (Iter);
         Store.Set (Iter, 0, Table.Name);
      end loop;
      View.Set_Model (Store.To_Interface);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
      Gtk.Tree_View_Column.Gtk_New (Text_Column);
      Num := View.Append_Column (Text_Column);
      Text_Column.Pack_Start (Text_Render, True);
      Text_Column.Set_Sizing
        (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
      Text_Column.Set_Title ("Name");
      Text_Column.Add_Attribute (Text_Render, "text",
                                 Glib.Gint (0));

      Tree_View_Callback.Connect
        (View, Gtk.Tree_View.Signal_Row_Activated,
         Select_Table_Handler'Access,
         Kit_UI (UI));

   end Fill_Table_List;

   --------------------------
   -- Select_Table_Handler --
   --------------------------

   procedure Select_Table_Handler
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      UI     : Kit_UI)
   is
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
        Widget.Get_Selection;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.Tree_Selection.Get_Selected (Selection, Model, Iter);
      UI.Show_Table (Gtk.Tree_Model.Get_String (Model, Iter, 0));
   end Select_Table_Handler;

end Kit.UI.Gtk_UI.Table_Lists;
