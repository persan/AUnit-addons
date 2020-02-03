with AUnit.Assertions.Generic_Helpers;
package AUnit.Assertions.Typed is

   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Integer_Image (Short_Short_Integer);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Integer_Image (Short_Integer);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Integer_Image (Integer);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Integer_Image (Long_Integer);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Integer_Image (Long_Long_Integer);

   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Float_Image (Short_Float);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Float_Image (Float);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Float_Image (Long_Float);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Float_Image (Long_Long_Float);
   procedure Assert is new AUnit.Assertions.Generic_Helpers.Assert_Enumeration_Image (Boolean);

end AUnit.Assertions.Typed;
