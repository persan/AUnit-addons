with AUnit.Test_Cases;
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;
with GNAT.Source_Info;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;

generic
   type Fixture is new AUnit.Test_Fixtures.Test_Fixture with private;
package AUnit.Suite_Builders_Generic is

   type Builder is tagged limited private;
   --
   --  Simplifies the construction of a test suite for an AUnit Test Fixture by
   --  wrapping the Test_Caller.
   --
   --  The builder ensures all tests are added with a consistent naming
   --  convention including the suite name as a prefix.

   function To_Suite
     (This : in Builder)
      return not null AUnit.Test_Suites.Access_Test_Suite;
   --
   --  Returns the constructed test suite.

   procedure Set_Suite_Name
     (This : in out Builder;
      Name : in String);
   --
   --  Override the default suite name.
   --  Normally this operation should not need to be used as the suite name is
   --  derived from the context of the generic instantiation.

   procedure Add
     (This : in out Builder;
      Name : in String;
      Test : access procedure (T : in out Fixture));
   --
   --  Add a single test operation to the suite.

   procedure Add
     (This : in out Builder;
      Test : not null access AUnit.Test_Cases.Test_Case'Class);
   --
   --  Add a test case to the suite.
   --  Exceptions: None

   procedure Add
     (This  : in out Builder;
      Suite : not null access AUnit.Test_Suites.Test_Suite'Class);
   --
   --  Adds a nested test suite.

private

   type Builder is limited new
     Ada.Finalization.Limited_Controlled with
      record
         M_Prefix : Ada.Strings.Unbounded.Unbounded_String :=
                      Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Source_Info.Enclosing_Entity);
         M_Suite  : AUnit.Test_Suites.Access_Test_Suite;
      end record;

   overriding
   procedure Initialize
     (This : in out Builder);

end AUnit.Suite_Builders_Generic;
