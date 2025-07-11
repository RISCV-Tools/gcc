------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . C A S E _ U T I L                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2025, AdaCore                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body System.Case_Util
  with SPARK_Mode
is
   --------------
   -- To_Lower --
   --------------

   function To_Lower (A : String) return String is
      Result : String := A;
   begin
      To_Lower (Result);
      return Result;
   end To_Lower;

   --------------
   -- To_Mixed --
   --------------

   function To_Mixed (A : String) return String is
      Result : String := A;
   begin
      To_Mixed (Result);
      return Result;
   end To_Mixed;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (A : String) return String is
      Result : String := A;
   begin
      To_Upper (Result);
      return Result;
   end To_Upper;

end System.Case_Util;
