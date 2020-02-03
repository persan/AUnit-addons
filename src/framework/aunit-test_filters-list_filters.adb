pragma Ada_2012;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Regpat;
with AUnit.Test_Cases;
package body AUnit.Test_Filters.List_Filters is
   use Ada.Text_IO;
   use GNAT.Regpat;


   function Is_Active
     (Filter : List_Filter;
      T      : AUnit.Tests.Test'Class) return Boolean is
   begin
      Filter.All_Tests.Include (AUnit.Test_Cases.Test_Case'Class (T).Name.all);
      return Filter.Units.Contains (AUnit.Test_Cases.Test_Case'Class (T).Name.all);
   end;

   ----------
   -- Read --
   ----------

   procedure Read (Self : in out List_Filter; Path : String) is
      Matcher : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("^([1-9a-zA-Z\._]+).*");
      Matches : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
      Inf     : File_Type;
   begin
      Open (Inf , In_file, Path);
      while not End_Of_File (Inf) loop
         declare
            Line : constant String := Ada.Strings.Fixed.Trim (Ada.Text_IO.Get_Line(Inf), Ada.Strings.Both);
         begin
            GNAT.Regpat.Match (Matcher, Line, Matches);
            if Matches (Matches'First) /= No_Match then
               Self.Units.Include (Line (Matches (Matches'First).First .. Matches (Matches'First).Last));
            end if;
         end;
      end loop;
      Close (Inf);
   end Read;

   procedure Write (Self : in out List_Filter;
                    Path : String) is
      Outf     : File_Type;
   begin
      Create (Outf , Out_File, Path);
      for I of Self.All_Tests.all loop
         Put_Line (Outf, I);
      end loop;
      Close (Outf);
   end;

end AUnit.Test_Filters.List_Filters;
