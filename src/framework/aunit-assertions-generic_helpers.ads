package AUnit.Assertions.Generic_Helpers is

   procedure Assert_Error
     (Message   : String := "";
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Object_Type is private;
   procedure Assert_Private
     (Actual    : Object_Type;
      Expected  : Object_Type;
      Message   : String := "";
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Object_Type is private;
      with function Image (Item : Object_Type) return String is <>;
   procedure Assert_Private_Image
     (Actual    : Object_Type;
      Expected  : Object_Type;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Object_Type is limited private;
      with function Equal (L, R : Object_Type) return Boolean;
   procedure Assert_Limited_Private
     (Actual    : Object_Type;
      Expected  : Object_Type;
      Message   : String := "";
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Object_Type is limited private;
      with function Equal (L, R : Object_Type) return Boolean;
      with function Image (Item : Object_Type) return String is <>;
   procedure Assert_Limited_Private_Image
     (Actual    : Object_Type;
      Expected  : Object_Type;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   Default_Tolerance : constant := 1.0E-5;

   generic
      type Num is digits <>;
   procedure Assert_Float
     (Actual    : Num;
      Expected  : Num;
      Tolerance : Num := Default_Tolerance;
      Message   : String := "";
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Num is digits <>;
   procedure Assert_Float_Image
     (Actual    : Num;
      Expected  : Num;
      Tolerance : Num := Default_Tolerance;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Num is range <>;
   procedure Assert_Integer
     (Actual    : Num;
      Expected  : Num;
      Message   : String := "";
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Num is range <>;
   procedure Assert_Integer_Image
     (Actual    : Num;
      Expected  : Num;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Num is mod <>;
   procedure Assert_Modular
     (Actual    : Num;
      Expected  : Num;
      Message   : String := "";
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Num is mod <>;
   procedure Assert_Modular_Image
     (Actual    : Num;
      Expected  : Num;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Enum is (<>);
   procedure Assert_Enumeration
     (Actual    : Enum;
      Expected  : Enum;
      Message   : String := "";
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   generic
      type Enum is (<>);
   procedure Assert_Enumeration_Image
     (Actual    : Enum;
      Expected  : Enum;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

end AUnit.Assertions.Generic_Helpers;
