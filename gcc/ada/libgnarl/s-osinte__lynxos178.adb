------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2025, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  Version of System.OS_Interface for LynxOS-178 (POSIX Threads)

package body System.OS_Interface is

   ------------------
   --  Current_CPU --
   ------------------

   function Current_CPU return Multiprocessors.CPU is
   begin
      --  No multiprocessor support, always return the first CPU Id

      return Multiprocessors.CPU'First;
   end Current_CPU;

   --------------------
   --  Get_Affinity  --
   --------------------

   function Get_Affinity (Id : Thread_Id) return Multiprocessors.CPU_Range is
      pragma Unreferenced (Id);

   begin
      --  No multiprocessor support, always return Not_A_Specific_CPU

      return Multiprocessors.Not_A_Specific_CPU;
   end Get_Affinity;

   ---------------
   --  Get_CPU  --
   ---------------

   function Get_CPU  (Id : Thread_Id) return Multiprocessors.CPU is
      pragma Unreferenced (Id);

   begin
      --  No multiprocessor support, always return the first CPU Id

      return Multiprocessors.CPU'First;
   end Get_CPU;

   -------------------
   -- Get_Page_Size --
   -------------------

   SC_PAGESIZE : constant := 17;
   --  C macro to get pagesize value from sysconf

   function sysconf (name : int) return long;
   pragma Import (C, sysconf, "sysconf");

   function Get_Page_Size return int is
   begin
      return int (sysconf (SC_PAGESIZE));
   end Get_Page_Size;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
   end To_Duration;

   ------------------------
   -- To_Target_Priority --
   ------------------------

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int
   is
   begin
      return Interfaces.C.int (Prio);
   end To_Target_Priority;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;

   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F is negative due to a round-up, adjust for positive F value

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return timespec'(tv_sec => S,
                       tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -------------
   -- sigwait --
   -------------

   function sigwait
     (set :  access sigset_t;
      sig :  access Signal)
      return int
   is
      function sigwaitinfo
        (set   : access sigset_t;
         info  : System.Address) return Signal;
      pragma Import (C, sigwaitinfo, "sigwaitinfo");

   begin
      sig.all := sigwaitinfo (set, Null_Address);

      if sig.all = -1 then
         return errno;
      end if;

      return 0;
   end sigwait;

   --------------------
   -- Get_Stack_Base --
   --------------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Warnings (Off, thread);
   begin
      return Null_Address;
   end Get_Stack_Base;

   ------------------
   -- pthread_init --
   ------------------

   procedure pthread_init is
   begin
      null;
   end pthread_init;

end System.OS_Interface;
