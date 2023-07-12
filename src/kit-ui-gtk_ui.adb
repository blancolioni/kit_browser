with Ada.Containers.Vectors;
with Ada.Text_IO;

with Glib.Error;
with Glib.Main;

with Gdk.Pixbuf;
with Gdk.Threads;

with Gtk.Builder;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Notebook;
with Gtk.Scrolled_Window;
with Gtk.Tree_View;
with Gtk.Window;

with Kit.Db.Database;
with Kit.Install;

with Kit.UI.Gtk_UI.Table_Book;
with Kit.UI.Gtk_UI.Table_Lists;

package body Kit.UI.Gtk_UI is

   package Window_Callback is
      new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   package List_Of_Table_Displays is
     new Ada.Containers.Vectors (Positive, Table_Book.Table_Display,
                                 Table_Book."=");

   Local_UI : Kit_UI;

   Timeout_Id : Glib.Main.G_Source_Id;

   function Timeout_Handler return Boolean;

   type Table_Display_Access is
     access List_Of_Table_Displays.Vector;

   type Root_Gtk_UI is
     new Root_Kit_UI
   with
      record
         Top_Level      : Gtk.Window.Gtk_Window;
         Table_List     : Gtk.Tree_View.Gtk_Tree_View;
         Table_Book     : Gtk.Notebook.Gtk_Notebook;
         Table_Displays : Table_Display_Access;
      end record;

   overriding function Choose_Database
     (With_UI : Root_Gtk_UI)
      return String;

   overriding procedure Show_Table
     (With_UI    : Root_Gtk_UI;
      Table_Name : String);

   overriding procedure Start (UI   : Root_Gtk_UI;
                    Path : String);

   ---------------------
   -- Choose_Database --
   ---------------------

   overriding function Choose_Database
     (With_UI : Root_Gtk_UI)
      return String
   is
      pragma Unreferenced (With_UI);
   begin
      return "";
   end Choose_Database;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
      use type Glib.Main.G_Source_Id;
   begin
      Kit.Db.Database.Close;
      if Timeout_Id /= 0 then
         Glib.Main.Remove (Timeout_Id);
      end if;
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   ----------------
   -- New_Gtk_UI --
   ----------------

   function New_Gtk_UI return Kit_UI is
      Result : Root_Gtk_UI;
      Builder : Gtk.Builder.Gtk_Builder;
   begin
      Gdk.Threads.G_Init;
      Gdk.Threads.Init;

      Gtk.Main.Init;
      Gtk.Builder.Gtk_New (Builder);

      declare
         use type Glib.Guint;
         Path  : constant String :=
                   Kit.Install.Library_Path
                   & "/ui.glade";
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            raise Program_Error with
              "Error opening GUI definition: " & Path
              & ": " & Glib.Error.Get_Message (Error);
         end if;
      end;

      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
                         Gtk.Window.Gtk_Window
                           (Builder.Get_Object
                              ("Kit_Main"));
         Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf;
         Error       : Glib.Error.GError;
      begin

         Gdk.Pixbuf.Gdk_New_From_File
           (Pixbuf,
            Kit.Install.Library_Path & "/kit.png",
            Error);
         Main_Window.Set_Icon (Pixbuf);
         Main_Window.Maximize;
         Result.Top_Level := Main_Window;
         Window_Callback.Connect
           (Main_Window,
            "destroy",
            Window_Callback.To_Marshaller (Destroy_Handler'Access));
      end;

      Result.Table_List :=
        Gtk.Tree_View.Gtk_Tree_View
          (Builder.Get_Object ("Table_List"));

      Result.Table_Book :=
        Gtk.Notebook.Gtk_Notebook
          (Builder.Get_Object ("Table_Book"));

      Result.Table_Displays := new List_Of_Table_Displays.Vector;

      return new Root_Gtk_UI'(Result);
   end New_Gtk_UI;

   ----------------
   -- Show_Table --
   ----------------

   overriding procedure Show_Table
     (With_UI    : Root_Gtk_UI;
      Table_Name : String)
   is
   begin
      Ada.Text_IO.Put_Line ("Table: " & Table_Name);
      for I in 1 .. With_UI.Table_Displays.Last_Index loop
         if With_UI.Table_Displays.Element (I).Table_Name = Table_Name then
            With_UI.Table_Book.Set_Current_Page (Glib.Gint (I - 1));
            return;
         end if;
      end loop;

      declare
         Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
         Display  : constant Kit.UI.Gtk_UI.Table_Book.Table_Display :=
                      Kit.UI.Gtk_UI.Table_Book.New_Table_Display
                        (Table_Name);
         Label  : Gtk.Label.Gtk_Label;
      begin
         Gtk.Scrolled_Window.Gtk_New
           (Scrolled);
         Scrolled.Add (Display.Widget);
         Scrolled.Show_All;
         Gtk.Label.Gtk_New (Label, Table_Name);
         Label.Show_All;
         With_UI.Table_Book.Append_Page (Scrolled, Label);
         With_UI.Table_Displays.Append (Display);
         With_UI.Table_Book.Set_Current_Page;
      end;

      declare
         use type Glib.Main.G_Source_Id;
      begin
         if Timeout_Id = 0 then
            Timeout_Id := Glib.Main.Idle_Add (Timeout_Handler'Access);
         end if;
      end;

   end Show_Table;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI   : Root_Gtk_UI;
                    Path : String)
   is
   begin
      UI.Top_Level.Show_All;
      Kit.Db.Database.Open (Path);
      Kit.UI.Gtk_UI.Table_Lists.Fill_Table_List
        (UI'Unchecked_Access, UI.Table_List);
      Timeout_Id := Glib.Main.Idle_Add (Timeout_Handler'Access);
--        Timeout_Id := Glib.Main.Timeout_Add
--          (Interval => 1000,
--           Func     => Timeout_Handler'Access);

      Local_UI := UI'Unchecked_Access;

      Gdk.Threads.Enter;
      Gtk.Main.Main;
      Gdk.Threads.Leave;

   end Start;

   -------------------
   -- Start_Updates --
   -------------------

   procedure Start_Updates is
      use type Glib.Main.G_Source_Id;
   begin
      if Timeout_Id = 0 then
         Timeout_Id := Glib.Main.Idle_Add (Timeout_Handler'Access);
      end if;
   end Start_Updates;

   ---------------------
   -- Timeout_Handler --
   ---------------------

   function Timeout_Handler return Boolean is
      Continue : Boolean := False;
   begin
      for Display of Root_Gtk_UI (Local_UI.all).Table_Displays.all loop
         declare
            Changed : Boolean;
         begin
            Display.Update (100, Changed);
            if Changed then
               Continue := True;
            end if;
         end;
      end loop;
      if not Continue then
         Timeout_Id := 0;
      end if;
      return Continue;
   end Timeout_Handler;

end Kit.UI.Gtk_UI;
