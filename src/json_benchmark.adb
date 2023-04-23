--    Copyright (C) 2023 A.J. Ianozi <aj@ianozi.com>
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Calendar;           use Ada.Calendar;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with GNATCOLL.JSON;
with JSON.Parsers;
with JSON.Types;

procedure Json_Benchmark is

   Invalid_Args : exception;

   --  Our json File Name.
   Json_Path : constant String :=
              (if Argument_Count = 1 then Argument (1) else
               raise Invalid_Args with
                  "Usage: ./json_benchmark ""file.json""");
   F : File_Type;

   Contents : Unbounded_String := Null_Unbounded_String;

   --  Timings
   Start_Time  : Time;
   End_Time    : Time;
   Seconds : Duration;

begin

   --  Read in the json file in.
   --  I don't know how to read gnatcoll.json from a file :)
   --  But I will not penalize gnatcoll.json for this.
   Open (F, In_File, Json_Path);
   while not End_Of_File (F) loop
      Append (Contents, Get_Line (F));
      Append (Contents, CR & LF);
   end loop;
   Close (F);

   --  json-ada
   Put_Line ("Testing json-ada's JSON");
   Start_Time := Clock;
   Json_Ada_Test :
   declare
      package Types is new JSON.Types
         (Long_Integer, Long_Float, Maximum_Number_Length => 1000);
      package Parsers is new JSON.Parsers
         (Types, Default_Maximum_Depth => 100000);
      Parser : Parsers.Parser := Parsers.Create_From_File (Json_Path);
      Result : constant Types.JSON_Value := Parser.Parse;
      use Parsers; use Types;
   begin
      End_Time := Clock;
      Seconds := (End_Time - Start_Time);
      Put_Line ("json-ada read is " & Seconds'Image & " seconds");
      --  Iterate
      Start_Time := Clock;
      declare
         Random_Node : constant JSON_Value := Result.Get ("Sixteen");
      begin
         for X of Random_Node loop
            declare
               Random_String : constant String := X.Get ("id").Value;
               Random_Int    : constant Long_Integer :=
                                          X.Get ("actor").Get ("id").Value;
               Another_Int : Long_Integer;
            begin
               Another_Int := Random_Int + 1;
            end;
         end loop;
         End_Time := Clock;
         Seconds := (End_Time - Start_Time);
         Put_Line ("json-ada iterate is " & Seconds'Image & " seconds");
      end;
   end Json_Ada_Test;

   --  gnatcoll.json
   Put_Line ("Testing GNATCOLL's JSON");
   Start_Time := Clock;
   GNATCOLL_Test :
   declare
      use GNATCOLL.JSON;
      Result : constant JSON_Value := Read (Contents);
   begin
      End_Time := Clock;
      Seconds := (End_Time - Start_Time);
      Put_Line ("GNATCOLL.JSON read is " & Seconds'Image & " seconds");
      --  Iterate
      Start_Time := Clock;
      declare
         Random_Node : constant JSON_Array := Result.Get ("Sixteen");
      begin
         for X of Random_Node loop
            declare
               Random_String : constant String := X.Get ("id");
               Random_Int    : constant Integer := X.Get ("actor").Get ("id");
               Another_Int : Integer;
            begin
               Another_Int := Random_Int + 1;
            end;
         end loop;
         End_Time := Clock;
         Seconds := (End_Time - Start_Time);
         Put_Line ("GNATCOLL.JSON iterate is " & Seconds'Image & " seconds");
      end;
   end GNATCOLL_Test;

   --  You can add VSS Here if you want, I don't know how to do the above
   --  with VSS.json ^

end Json_Benchmark;
