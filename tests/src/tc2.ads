with AUnit.Test_Cases;
package Tc2 is
   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;


   function Name (Test : Test_Case) return AUnit.Message_String;
   procedure Register_Tests (Test : in out Test_Case);

end Tc2;
