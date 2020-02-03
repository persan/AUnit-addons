with Ada.Strings.Fixed;
with AUnit.Test_Caller;

package body AUnit.Suite_Builders_Generic is

   package Caller is new AUnit.Test_Caller (Fixture);

   Package_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   Separator : constant String := " : ";
   --
   --  The separator for suite and test names.

   ---------------------------
   --  Compute_Suite_Prefix --
   ---------------------------
   procedure Compute_Suite_Prefix
     (This : in out Builder'Class)
   is
      Prefix       : String := Package_Name;
      Last_Dot     : Natural := 0;

      Test_Suffix  : constant String := ".Tests";
      Suffix_Index : Natural := 0;
   begin

      --  Trim off the name of this package.
      --
      Last_Dot := Ada.Strings.Fixed.Index
        (Source  => Prefix,
         Pattern => ".",
         Going   => Ada.Strings.Backward);

      if Last_Dot > 0 then
         Ada.Strings.Fixed.Delete
           (Source  => Prefix,
            From    => Last_Dot,
            Through => Prefix'Last);
      end if;

      --  Strip off a useless ".Test" suffix.
      --
      --  Note: Assumes the suffix doesn't also occur in the middle of the name.
      --
      Suffix_Index := Ada.Strings.Fixed.Index
        (Source  => Prefix,
         Pattern => Test_Suffix,
         Going   => Ada.Strings.Backward);

      if Suffix_Index > 0 then
         Ada.Strings.Fixed.Delete
           (Source  => Prefix,
            From    => Suffix_Index,
            Through => Prefix'Last);
      end if;

      This.M_Prefix := Ada.Strings.Unbounded.To_Unbounded_String
        (Ada.Strings.Fixed.Trim (Prefix, Ada.Strings.Both) & Separator);
   end Compute_Suite_Prefix;

   function To_Suite
     (This : in Builder)
      return not null AUnit.Test_Suites.Access_Test_Suite
   is
   begin
      return This.M_Suite;
   end To_Suite;

   procedure Set_Suite_Name
     (This : in out Builder;
      Name : in String)
   is
   begin
      This.M_Prefix := Ada.Strings.Unbounded.To_Unbounded_String (Name & Separator);
   end Set_Suite_Name;

   ----------------------------------------------------------------------------
   procedure Add
     (This : in out Builder;
      Name : in String;
      Test : access procedure (T : in out Fixture))
   is
   begin
      This.M_Suite.Add_Test
        (Caller.Create (Ada.Strings.Unbounded.To_String (This.M_Prefix) & Name, Test));
   end Add;

   ----------------------------------------------------------------------------
   procedure Add
     (This : in out Builder;
      Test : not null access AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      This.M_Suite.Add_Test (Test);
   end Add;

   ----------------------------------------------------------------------------
   procedure Add
     (This  : in out Builder;
      Suite : not null access AUnit.Test_Suites.Test_Suite'Class)
   is
   begin
      This.M_Suite.Add_Test (Suite);
   end Add;

   overriding
   procedure Initialize
     (This : in out Builder)
   is
   begin
      This.M_Suite := AUnit.Test_Suites.New_Suite;
      This.Compute_Suite_Prefix;
   end Initialize;

end AUnit.Suite_Builders_Generic;
