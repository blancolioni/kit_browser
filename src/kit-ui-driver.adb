with Ada.Command_Line;

with Kit.UI.Gtk_UI;

procedure Kit.UI.Driver is
   GUI : constant Kit_UI := Kit.UI.Gtk_UI.New_Gtk_UI;
begin
   if Ada.Command_Line.Argument_Count = 0 then
      GUI.Start ("");
   else
      GUI.Start (Ada.Command_Line.Argument (1));
   end if;
end Kit.UI.Driver;
