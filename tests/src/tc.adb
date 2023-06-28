package body Tc is

   use AUnit;

   function Name (Test : Test_Case) return AUnit.Message_String is
   begin
      return Format ("test");
   end Name;

   procedure Test_Routine (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;

   procedure Test_Routine2 (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;

   procedure Test_Routine4 (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;

   overriding procedure Register_Tests (Test : in out Test_Case) is separate;

end Tc;
