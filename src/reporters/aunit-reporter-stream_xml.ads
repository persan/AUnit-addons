------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . X M L                    --
--                                                                          --
--                                 S p e c                                  --
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

--  This is a xUnit reporter with output compatible with jUnit capable to
--  catch stdout and stderr and report to Streams.

with Ada.Streams;

private with Ada_Containers;
private with AUnit.Time_Measure;
private with Ada.Text_IO.Text_Streams;
private with GNAT.OS_Lib;

package AUnit.Reporter.Stream_XML is

   type XML_Reporter is new Reporter with private;

   type Stream_Access is not null access all Ada.Streams.Root_Stream_Type'Class;

   procedure Set_Output (Engine  : in out XML_Reporter; Stream : Stream_Access);
   --  Set the stream for the report.

   procedure Catch_Output_And_Error (Engine  : in out XML_Reporter);
   --  A call to this routine will redirct Standard_Output and Standard_Error
   --  to temporary files and include the contents of these files
   --  in the final testreport.

   procedure Report (Engine  : XML_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options);

private
   type XML_Reporter is new Reporter with record
      Output  : Stream_Access := Stream_Access (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output));

      Stdout        : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Stderr        : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;

      Stdout_Name   : GNAT.OS_Lib.String_Access;
      Stderr_Name   : GNAT.OS_Lib.String_Access;
   end record;
   function Image (S : Ada_Containers.Count_Type) return String;

   function Image (S : Integer) return String;

   function Image (S : AUnit.Time_Measure.AUnit_Duration) return String;

end AUnit.Reporter.Stream_XML;
