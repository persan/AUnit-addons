--  ----------------------------------------------------------------------------
--
--  The intent of this unit is to provide a simple main program that runs
--  one Test_Suite.
--  The procedure is intended to be instansiated as a childern to
--  the package containing the function Suite.
--
--  with AUnit.Test_Suites.Simple_Main_Generic;
--  procedure component.children.Tests.Suit1.main is
--     new AUnit.Test_Suites.Simple_Main_Generic(Suit);
--
--  Thus providing a simple main for One_Testcase to be used during development.
--
--  ----------------------------------------------------------------------------
with AUnit.Run.Generic_Runner;
generic
procedure AUnit.Test_Suites.Simple_Main_Generic renames AUnit.Run.Generic_Runner;
