with AUnit.Test_Suites;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Fixed.Hash;
package AUnit.Test_Filters.List_Filters is

   type List_Filter is new AUnit.Test_Filters.Name_Filter with private;

   procedure Read (Self : in out List_Filter; Path : String);
   ---
   -- Reads a textfile containing tests to run
   -- Lines beginning with  --, #, and ; are ignorerd
   ---
   ----------------------------------------------

   procedure Write (Self : in out List_Filter;
                    Path : String);
      ---
      --  Writes all testcases in a Test_Suite to file.
      -- ---------------------------------------------

   function Is_Active
     (Filter : List_Filter;
      T      : AUnit.Tests.Test'Class) return Boolean;


private
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type => String,
      Hash         => Ada.Strings.Fixed.Hash ,
      Equivalent_Elements => "=" );
   type String_Set_Access is access all String_Sets.Set;
   type List_Filter is new AUnit.Test_Filters.Name_Filter with record
      Units     : String_Sets.Set;
      All_Tests : String_Set_Access := new String_Sets.Set;
   end record;
end AUnit.Test_Filters.List_Filters;
