--  begin read only
separate (Tc)
overriding procedure Register_Tests (Test : in out Test_Case) is
   use AUnit.Test_Cases.Registration;
begin
   Register_Routine (Test, Test_Routine'Unrestricted_Access, "Test_Routine");
   Register_Routine (Test, Test_Routine33'Unrestricted_Access, "Test_Routine33");
   Register_Routine (Test, Test_Routine4'Unrestricted_Access, "Test_Routine4");
end Register_Tests;
--  end read only
