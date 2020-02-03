pragma Ada_2012;
package body AUnit.Assertions.Generic_Helpers is

   ------------------
   -- Assert_Error --
   ------------------

   procedure Assert_Error
     (Message : String  := ""; Source : String := GNAT.Source_Info.File;
      Line    : Natural := GNAT.Source_Info.Line)
   is
   begin
      AUnit.Assertions.Assert (False, Message, Source, Line);
   end Assert_Error;

   --------------------
   -- Assert_Private --
   --------------------

   procedure Assert_Private
     (Actual   : Object_Type;
      Expected : Object_Type;
      Message  : String := "";
      Source   : String  := GNAT.Source_Info.File;
      Line     : Natural := GNAT.Source_Info.Line)
   is
   begin
      AUnit.Assertions.Assert (Actual /= Expected, Message, Source, Line);
   end Assert_Private;

   --------------------------
   -- Assert_Private_Image --
   --------------------------

   procedure Assert_Private_Image
     (Actual : Object_Type; Expected : Object_Type;
      Source : String  := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if Actual /= Expected then
         Assert_Error ("Actual => (" & Image (Actual) & "), " &
                         "Excpected=>(" & Image (Expected) & ")", Source, Line);
      end if;
   end Assert_Private_Image;

   ----------------------------
   -- Assert_Limited_Private --
   ----------------------------

   procedure Assert_Limited_Private
     (Actual : Object_Type; Expected : Object_Type; Message : String := "";
      Source : String  := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not Equal (Actual, Expected) then
         Assert_Error (Message, Source, Line);
      end if;
   end Assert_Limited_Private;

   ----------------------------------
   -- Assert_Limited_Private_Image --
   ----------------------------------

   procedure Assert_Limited_Private_Image
     (Actual : Object_Type; Expected : Object_Type;
      Source : String  := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not Equal (Actual, Expected) then
         Assert_Error ("Actual => (" & Image (Actual) & "), " &
                         "Excpected=>(" & Image (Expected) & ")", Source, Line);
      end if;
   end Assert_Limited_Private_Image;

   ------------------
   -- Assert_Float --
   ------------------

   procedure Assert_Float
     (Actual    : Num;
      Expected  : Num;
      Tolerance : Num := Default_Tolerance;
      Message   : String  := "";
      Source    : String := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if abs (Actual - Expected) > Tolerance then
         Assert_Error (Message, Source, Line);
      end if;
   end Assert_Float;

   ------------------------
   -- Assert_Float_Image --
   ------------------------

   procedure Assert_Float_Image
     (Actual : Num; Expected : Num; Tolerance : Num := Default_Tolerance;
      Source : String  := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if abs (Actual - Expected) > Tolerance then
         Assert_Error ("Actual => (" & Actual'Img & "), " &
                         "Excpected=>(" & Expected'Img & ")", Source, Line);
      end if;
   end Assert_Float_Image;

   --------------------
   -- Assert_Integer --
   --------------------

   procedure Assert_Integer
     (Actual : Num; Expected : Num; Message : String := "";
      Source : String  := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if Actual /= Expected then
         Assert_Error (Message, Source, Line);
      end if;
   end Assert_Integer;

   --------------------------
   -- Assert_Integer_Image --
   --------------------------

   procedure Assert_Integer_Image
     (Actual : Num; Expected : Num; Source : String := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if abs Actual /= Expected then
         Assert_Error ("Actual => (" & Actual'Img & "), " &
                         "Excpected=>(" & Expected'Img & ")", Source, Line);
      end if;
   end Assert_Integer_Image;

   --------------------
   -- Assert_Modular --
   --------------------

   procedure Assert_Modular
     (Actual : Num; Expected : Num; Message : String := "";
      Source : String  := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if Actual /= Expected then
         Assert_Error (Message, Source, Line);
      end if;
   end Assert_Modular;

   --------------------------
   -- Assert_Modular_Image --
   --------------------------

   procedure Assert_Modular_Image
     (Actual : Num; Expected : Num; Source : String := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if abs Actual /= Expected then
         Assert_Error ("Actual => (" & Actual'Img & "), " &
                         "Excpected=>(" & Expected'Img & ")", Source, Line);
      end if;
   end Assert_Modular_Image;

   ------------------------
   -- Assert_Enumeration --
   ------------------------

   procedure Assert_Enumeration
     (Actual : Enum; Expected : Enum; Message : String := "";
      Source : String  := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if Actual /= Expected then
         Assert_Error (Message, Source, Line);
      end if;
   end Assert_Enumeration;

   ------------------------------
   -- Assert_Enumeration_Image --
   ------------------------------

   procedure Assert_Enumeration_Image
     (Actual : Enum; Expected : Enum; Source : String := GNAT.Source_Info.File;
      Line   : Natural := GNAT.Source_Info.Line)
   is
   begin
      if Actual /= Expected then
         Assert_Error ("Actual => (" & Actual'Img & "), " &
                         "Excpected=>(" & Expected'Img & ")", Source, Line);
      end if;
   end Assert_Enumeration_Image;

end AUnit.Assertions.Generic_Helpers;
