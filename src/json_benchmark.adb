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
pragma Ada_2022;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;     use Ada.Characters.Latin_1;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with JSON.Streams;

with VSS.Strings;
with VSS.JSON;
with VSS.JSON.Streams;
with VSS.JSON.Pull_Readers;
with VSS.JSON.Pull_Readers.Simple;
with VSS.Stream_Element_Vectors;
with VSS.Strings.Conversions;
with VSS.Strings.Converters;
with VSS.Strings.Converters.Encoders;
--with VSS.String_Vectors;
with VSS.Text_Streams.Memory_UTF8_Input;
--with VSS.Stream_Element_Vectors.Conversions;

with GNATCOLL.JSON;
with JSON.Parsers;
with JSON.Types;

procedure Json_Benchmark is

   Invalid_Args : exception;
   Invalid_File : exception;

   --  The special key and values in the large-file.json file
   True_Random_Key     : constant String       := "Sixteen";
   True_Random_Key_VSS : constant VSS.Strings.Virtual_String := "Sixteen";

   --  Our json File Name.
   JSON_Path : constant String :=
              (if Argument_Count = 1 then Argument (1) else
               raise Invalid_Args with
                  "Usage: ./json_benchmark ""file.json""");

   --  To read in the json file in.
   --  I don't know how to read gnatcoll.json or vss from a file :)
   --  But I will not penalize gnatcoll.json for this.
   function Read_File (File_Path : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
      F : File_Type;
   begin
      Open (F, In_File, File_Path);
      while not End_Of_File (F) loop
         Append (Result, Get_Line (F));
         Append (Result, CR & LF);
      end loop;
      Close (F);
      return To_String (Result);
   end Read_File;
   --  For gnatcoll
   JSON_String : constant String := Read_File (JSON_Path);

   --  for VSS
   JSON_VSS    : constant VSS.Strings.Virtual_String :=
                     VSS.Strings.To_Virtual_String
                        (To_Wide_Wide_String (JSON_String));

   --  For json_ada (I cannot just pass the string due to stack overflow)
   JSON_Stream : constant JSON.Streams.Stream_Element_Array_Access :=
                           JSON.Streams.From_File (JSON_Path);

   --  Timings
   Start_Time  : Time;
   End_Time    : Time;
   Seconds : Duration;

begin

   --  json-ada
   Put_Line ("Testing json-ada's JSON");
   Start_Time := Clock;
   Json_Ada_Test :
   declare
      package Types is new JSON.Types
         (Long_Integer, Long_Float, Maximum_Number_Length => 1000);
      package Parsers is new JSON.Parsers
         (Types, Default_Maximum_Depth => 100000);
      --  I can only either read from a file or use a stream element array
      --  Otherwise I get a stackoverflow with JSON_Ada
      Parser : Parsers.Parser := Parsers.Create (JSON_Stream);
      Result : constant Types.JSON_Value := Parser.Parse;
      use Types;
   begin
      End_Time := Clock;
      Seconds := (End_Time - Start_Time);
      Put_Line ("json-ada read is " & Seconds'Image & " seconds");
      --  Seek
      Start_Time := Clock;
      declare
         Random_Node : constant JSON_Value := Result.Get (True_Random_Key);
      begin
         End_Time := Clock;
         Seconds := (End_Time - Start_Time);
         Put_Line ("json-ada seek is " & Seconds'Image & " seconds");
         -- Iterate
         Start_Time := Clock;
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
         Put_Line ("json-ada iter is " & Seconds'Image & " seconds");
      end;
   end Json_Ada_Test;

   --  gnatcoll.json
   Put_Line ("Testing GNATCOLL's JSON");
   Start_Time := Clock;
   GNATCOLL_Test :
   declare
      use GNATCOLL.JSON;
      Result : constant JSON_Value := Read (JSON_String);
   begin
      End_Time := Clock;
      Seconds := (End_Time - Start_Time);
      Put_Line ("GNATCOLL.JSON read is " & Seconds'Image & " seconds");
      --  Iterate
      Start_Time := Clock;
      declare
         Random_Node : constant JSON_Array := Result.Get (True_Random_Key);
      begin
         --  Seek
         End_Time := Clock;
         Seconds := (End_Time - Start_Time);
         Put_Line ("GNATCOLL.JSON seek is " & Seconds'Image & " seconds");
         -- Iterate
         Start_Time := Clock;
         for X of Random_Node loop
            declare
               Random_String : constant String := X.Get ("id");
               Random_Int    : constant Long_Integer :=
                                 X.Get ("actor").Get ("id");
               Another_Int : Long_Integer;
            begin
               Another_Int := Random_Int + 1;
            end;
         end loop;
         End_Time := Clock;
         Seconds := (End_Time - Start_Time);
         Put_Line ("GNATCOLL.JSON iter is " & Seconds'Image & " seconds");
      end;
   end GNATCOLL_Test;

   --  VSS
   --  This one was complicated because VSS's json doesn't seem to just load
   --  everything into a dictionary.  It's much more low level. PLEASE let me
   --  know if VSS can just load things into a dictionary out-of-the-box :)
   Put_Line ("Testing VSS's JSON");
   Start_Time := Clock;
   VSS_Test :
   declare
      use VSS.Strings;
      use VSS.Strings.Converters;
      use VSS.Strings.Conversions;
      use VSS.Stream_Element_Vectors;
      use VSS.Strings.Converters.Encoders;
      use type VSS.JSON.Streams.JSON_Stream_Element_Kind;

      type Random_Object is record
         Random_String : VSS.Strings.Virtual_String;
         Random_Int    : Long_Integer;
      end record;

      procedure Skip_Object
         (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
      is
      begin
         loop
            case Reader.Read_Next is
               when VSS.JSON.Streams.Start_Object =>
                  Skip_Object (Reader);
               when VSS.JSON.Streams.End_Object |
                    VSS.JSON.Streams.End_Document =>
                  exit;

               when others =>
                  null;
            end case;
         end loop;
      end Skip_Object;

      procedure Skip_Array
         (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
      is
      begin
         loop
            case Reader.Read_Next is
               when VSS.JSON.Streams.Start_Array =>
                  Skip_Array (Reader);
               when VSS.JSON.Streams.End_Array |
                    VSS.JSON.Streams.End_Document =>
                  exit;

               when others =>
                  null;
            end case;
         end loop;
      end Skip_Array;

      procedure Read_Actor
         (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
          Item    : out Long_Integer) is
      begin
         loop
            case Reader.Read_Next is
               when VSS.JSON.Streams.Key_Name =>
                  if Reader.Key_Name = "id" then
                     case Reader.Read_Next is
                        when VSS.JSON.Streams.Number_Value =>
                           Item := Long_Integer
                              (VSS.JSON.As_Integer (Reader.Number_Value));
                        when others =>
                           raise Invalid_File;
                     end case;
                  end if;
               when VSS.JSON.Streams.End_Object =>
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end Read_Actor;

      procedure Read_Object
         (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
          Item    : out Random_Object) is
      begin
         loop
            case Reader.Read_Next is
               when VSS.JSON.Streams.Key_Name =>
                  if Reader.Key_Name = "id" then
                     case Reader.Read_Next is
                        when VSS.JSON.Streams.String_Value =>
                           Item.Random_String := Reader.String_Value;
                        when others =>
                           raise Invalid_File;
                     end case;
                  elsif Reader.Key_Name = "actor" then
                     Read_Actor (Reader, Item.Random_Int);
                  end if;
               when VSS.JSON.Streams.Start_Object =>
                  Skip_Object (Reader);
               when VSS.JSON.Streams.End_Object =>
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end Read_Object;

      procedure Iterate_Objects
         (Reader : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
      is
      begin
         loop
            case Reader.Read_Next is
               --  Start of the next object
               when VSS.JSON.Streams.Start_Object =>
                  declare
                     Another_Int : Long_Integer;
                     Next_Object : Random_Object :=
                        (Random_String => Empty_Virtual_String,
                         Random_Int    => 0);
                  begin
                     Read_Object (Reader, Next_Object);
                     Another_Int := Next_Object.Random_Int + 1;
                  end;

               when VSS.JSON.Streams.End_Array =>
                  exit;

               when others =>
                  null;
            end case;
         end loop;
      end Iterate_Objects;

      Encoder : Virtual_String_Encoder;
   begin
      Encoder.Initialize ("utf-8", [Stateless => True, others => False]);
      declare
         Items  : constant Stream_Element_Vector := Encoder.Encode (JSON_VSS);
         Stream : aliased
            VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;
         Reader : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      begin
         Stream.Set_Data (Items);
         Reader.Set_Stream (Stream'Unchecked_Access);
         End_Time := Clock;
         Seconds := (End_Time - Start_Time);
         Put_Line ("VSS.JSON read is " & Seconds'Image & " seconds");
         --  Seek
         Start_Time := Clock;
         if Reader.Read_Next /= VSS.JSON.Streams.Start_Document then
            raise Invalid_File;
         end if;
         loop
            case Reader.Read_Next is
               when VSS.JSON.Streams.Key_Name =>
                  if Reader.Key_Name = True_Random_Key_VSS then
                     --  Seek
                     End_Time := Clock;
                     Seconds := (End_Time - Start_Time);
                     Put_Line ("VSS.JSON seek is " &
                                 Seconds'Image & " seconds");
                     -- Iterate
                     Start_Time := Clock;
                     Iterate_Objects (Reader);
                     exit;
                  end if;
               when VSS.JSON.Streams.Start_Array =>
                  Skip_Array (Reader);
               when VSS.JSON.Streams.End_Document =>
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      end;
   end VSS_Test;
   End_Time := Clock;
   Seconds := (End_Time - Start_Time);
   Put_Line ("VSS.JSON iter is " & Seconds'Image & " seconds");

end Json_Benchmark;
