pragma Ada_2012;
with Tc;
package body Suits is

   T : aliased Tc.Test_Case;
   ----------
   -- Suit --
   ----------
   S : aliased AUnit.Test_Suites.Test_Suite;

   function Suit return AUnit.Test_Suites.Access_Test_Suite is
   begin
      S.Add_Test (T'Access);
      return S'Access;
   end Suit;

end Suits;
