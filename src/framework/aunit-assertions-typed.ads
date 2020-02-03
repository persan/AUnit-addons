with AUnit.Assertions.Generic_Helpers;

package AUnit.Assertions.Typed is

   procedure Assert is new Generic_Helpers.Assert_Integer_Image
     (Num => Short_Short_Integer);

   procedure Assert is new Generic_Helpers.Assert_Integer_Image
     (Num => Short_Integer);

   procedure Assert is new Generic_Helpers.Assert_Integer_Image
     (Num => Integer);

   procedure Assert is new Generic_Helpers.Assert_Integer_Image
     (Num => Long_Integer);

   procedure Assert is new Generic_Helpers.Assert_Integer_Image
     (Num => Long_Long_Integer);

   procedure Assert is new Generic_Helpers.Assert_Float_Image
     (Num => Short_Float);

   procedure Assert is new Generic_Helpers.Assert_Float_Image
     (Num => Float);

   procedure Assert is new Generic_Helpers.Assert_Float_Image
     (Num => Long_Float);

   procedure Assert is new Generic_Helpers.Assert_Float_Image
     (Num => Long_Long_Float);

   procedure Assert is new Generic_Helpers.Assert_Enumeration_Image
     (Enum => Boolean);

end AUnit.Assertions.Typed;
