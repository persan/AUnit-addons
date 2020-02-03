------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . X M L                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2013, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Time_Measure; use AUnit.Time_Measure;
with Ada_Containers; use Ada_Containers;

--  This is a xUnit reporter with output compatible with jUnit capable of
--  reporing to Streams.
package body AUnit.Reporter.Stream_XML is

   use Ada.Streams;
   use GNAT.OS_Lib;

   procedure Dump_Result_List (Engine  : XML_Reporter; L : Result_Lists.List);
   --  List failed assertions

   --   procedure Put_Measure is new Gen_Put_Measure;
   --  Output elapsed time

   procedure Report_Test (Engine  : XML_Reporter; Test : Test_Result);
   --  Report a single assertion failure or unexpected exception

   ------------------------------
   --  Catch_Output_And_Error  --
   ------------------------------
   procedure Catch_Output_And_Error (Engine  : in out XML_Reporter) is
   begin
      GNAT.OS_Lib.Close (GNAT.OS_Lib.Standout);
      GNAT.OS_Lib.Close (GNAT.OS_Lib.Standerr);
      GNAT.OS_Lib.Create_Temp_Output_File (Engine.Stdout, Engine.Stdout_Name);
      GNAT.OS_Lib.Create_Temp_Output_File (Engine.Stderr, Engine.Stderr_Name);
   end;

   ----------------------
   -- Dump_Result_List --
   ----------------------
   procedure Dump_Result_List (Engine  : XML_Reporter; L : Result_Lists.List) is
      use Result_Lists;
      C : Cursor := First (L);
   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Test (Engine, Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;

   -------------
   --  Image  --
   -------------
   function Image (S : Ada_Containers.Count_Type) return String is
      I : constant String := S'Img;
   begin
      return (if I (I'First) = ' ' then I (I'First + 1 .. I'Last) else I);
   end;

   -------------
   --  Image  --
   -------------
   function Image (S : Integer) return String is
      I : constant String := S'Img;
   begin
      return (if I (I'First) = ' 'then I (I'First + 1 .. I'Last) else I);
   end;

   -------------
   --  Image  --
   -------------
   function Image (S : AUnit_Duration) return String is
      Buffer : String (1 .. 40) := (others => ' ');
      Cursor : Natural := Buffer'First;
      procedure Put (S : String) is
      begin
         Buffer (Cursor .. Cursor + S'Length - 1) := S;
         Cursor := Cursor + S'Length;
      end;

      procedure Put (I : Integer) is
      begin
         Put (Image (I));
      end;
      procedure Put is new Gen_Put_Measure;
   begin
      Put (S);
      return Buffer (Buffer'First .. Cursor - 1);
   end;

   ------------------
   --  Set_Output  --
   ------------------
   procedure Set_Output (Engine  : in out XML_Reporter; Stream : Stream_Access) is
   begin
      Engine.Output := Stream;
   end;

   -------------
   --  Write  --
   -------------
   procedure Write (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
                    From_Path : String) is
      Last   : Integer;
      Fd     : File_Descriptor;
   begin
      Fd := Open_Read (From_Path, GNAT.OS_Lib.Binary);
      declare
         Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (File_Length (Fd)));
      begin
         Last := Read (Fd, Buffer'Address, Buffer'Length);
         Stream_Element_Array'Write (Stream, Buffer);
         if Last /= Buffer'Length then
            String'Write (Stream, "<<<  Warning Output is incopmplete>>>" & ASCII.LF);
         end if;
      end;
      GNAT.OS_Lib.Close (Fd);
   end;

   ------------
   -- Report --
   ------------
   procedure Report (Engine  : XML_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options)
   is
      T        : AUnit_Duration;
      Dummy_OK : Boolean;
   begin
      String'Write (Engine.Output, "<?xml version='1.0' encoding='utf-8' ?>" & ASCII.LF);
      String'Write (Engine.Output, "<TestRun");

      if Elapsed  (R) /= Time_Measure.Null_Time then
         T := Get_Measure (Elapsed (R));

         String'Write (Engine.Output, " elapsed=""");
         String'Write (Engine.Output, Image (T));
         String'Write (Engine.Output, """>" & ASCII.LF);
      else
         String'Write (Engine.Output, ">" & ASCII.LF);
      end if;

      String'Write (Engine.Output, "  <Statistics>" & ASCII.LF);
      String'Write (Engine.Output, "    <Tests>");
      String'Write (Engine.Output, Image (Test_Count (R)));
      String'Write (Engine.Output, "</Tests>" & ASCII.LF);
      String'Write (Engine.Output, "    <FailuresTotal>");
      String'Write (Engine.Output, Image (Integer (Failure_Count (R)) + Integer (Error_Count (R))));
      String'Write (Engine.Output, "</FailuresTotal>" & ASCII.LF);
      String'Write (Engine.Output, "    <Failures>");
      String'Write (Engine.Output, Image (Integer (Failure_Count (R))));
      String'Write (Engine.Output, "</Failures>" & ASCII.LF);
      String'Write (Engine.Output, "    <Errors>");
      String'Write (Engine.Output, Image (Integer (Error_Count (R))));
      String'Write (Engine.Output, "</Errors>" & ASCII.LF);
      String'Write (Engine.Output, "  </Statistics>" & ASCII.LF);

      declare
         S : Result_Lists.List;
      begin
         String'Write (Engine.Output, "  <SuccessfulTests>" & ASCII.LF);
         if Options.Report_Successes then
            Successes (R, S);
            Dump_Result_List (Engine, S);
         end if;
         String'Write (Engine.Output, "  </SuccessfulTests>" & ASCII.LF);
      end;

      String'Write (Engine.Output, "  <FailedTests>" & ASCII.LF);
      declare
         F : Result_Lists.List;
      begin
         Failures (R, F);
         Dump_Result_List (Engine, F);
      end;

      declare
         E : Result_Lists.List;
      begin
         Errors (R, E);
         Dump_Result_List (Engine, E);
      end;
      String'Write (Engine.Output, "  </FailedTests>" & ASCII.LF);

      if Engine.Stdout_Name /= null then
         GNAT.OS_Lib.Close (Engine.Stdout);
         GNAT.OS_Lib.Close (Engine.Stderr);
         String'Write (Engine.Output, "  <StandardOutput>");
         Write (Engine.Output, Engine.Stdout_Name.all);
         String'Write (Engine.Output, "</StandardOutput>" & ASCII.LF);
         String'Write (Engine.Output, "  <StandardError>");
         Write (Engine.Output, Engine.Stderr_Name.all);
         String'Write (Engine.Output, "</StandardError>" & ASCII.LF);
         GNAT.OS_Lib.Delete_File (Engine.Stderr_Name.all, Dummy_OK);
         GNAT.OS_Lib.Delete_File (Engine.Stdout_Name.all, Dummy_OK);
      end if;
      String'Write (Engine.Output, "</TestRun>" & ASCII.LF);
   end Report;

   ------------------
   -- Report_Test --
   ------------------
   procedure Report_Test (Engine  : XML_Reporter; Test : Test_Result) is
      Is_Assert : Boolean;
      T         : AUnit_Duration;
   begin
      String'Write (Engine.Output, "    <Test");

      if Test.Elapsed /= Time_Measure.Null_Time then
         T := Get_Measure (Test.Elapsed);

         String'Write (Engine.Output, " elapsed=""");
         String'Write (Engine.Output, Image (T));
         String'Write (Engine.Output, """>" & ASCII.LF);
      else
         String'Write (Engine.Output, ">" & ASCII.LF);
      end if;

      String'Write (Engine.Output, "      <Name>");
      String'Write (Engine.Output, Test.Test_Name.all);

      if Test.Routine_Name /= null then
         String'Write (Engine.Output, " : ");
         String'Write (Engine.Output, Test.Routine_Name.all);
      end if;

      String'Write (Engine.Output, "</Name>" & ASCII.LF);

      if Test.Failure /= null or else Test.Error /= null then
         if Test.Failure /= null then
            Is_Assert := True;
         else
            Is_Assert := False;
         end if;

         String'Write (Engine.Output, "      <FailureType>");

         if Is_Assert then
            String'Write (Engine.Output, "Assertion");
         else
            String'Write (Engine.Output, "Error");
         end if;

         String'Write (Engine.Output, "</FailureType>" & ASCII.LF);
         String'Write (Engine.Output, "      <Message>");
         if Is_Assert then
            String'Write (Engine.Output, Test.Failure.Message.all);
         else
            String'Write (Engine.Output, Test.Error.Exception_Name.all);
         end if;
         String'Write (Engine.Output, "</Message>" & ASCII.LF);

         if Is_Assert then
            String'Write (Engine.Output, "      <Location>" & ASCII.LF);
            String'Write (Engine.Output, "        <File>");
            String'Write (Engine.Output, Test.Failure.Source_Name.all);
            String'Write (Engine.Output, "</File>" & ASCII.LF);
            String'Write (Engine.Output, "        <Line>");
            String'Write (Engine.Output, Image (Test.Failure.Line));
            String'Write (Engine.Output, "</Line>" & ASCII.LF);
            String'Write (Engine.Output, "      </Location>" & ASCII.LF);

         else
            String'Write (Engine.Output, "      <Exception>" & ASCII.LF);
            String'Write (Engine.Output, "         <Message>");
            String'Write (Engine.Output, Test.Error.Exception_Name.all);
            String'Write (Engine.Output, "</Message>" & ASCII.LF);

            if Test.Error.Exception_Message /= null then
               String'Write (Engine.Output, "         <Information>");
               String'Write (Engine.Output, Test.Error.Exception_Message.all);
               String'Write (Engine.Output, "</Information>" & ASCII.LF);
            end if;

            if Test.Error.Traceback /= null then
               String'Write (Engine.Output, "         <Traceback>");
               String'Write (Engine.Output, Test.Error.Traceback.all);
               String'Write (Engine.Output, "</Traceback>" & ASCII.LF);
            end if;

            String'Write (Engine.Output, "      </Exception>" & ASCII.LF);
         end if;
      end if;
      String'Write (Engine.Output, "    </Test>" & ASCII.LF);
   end Report_Test;

end AUnit.Reporter.Stream_XML;
