package body Tc2 is

   use Aunit;

   function Name (Test : Test_Case) return AUnit.Message_String is
   begin
      return Format ("test");
   end Name;

   procedure Test_Routine (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;

   procedure Register_Tests (Test : in out Test_Case) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine (Test, Test_Routine'Access, "Test_Routine");
   end;

end Tc2;
