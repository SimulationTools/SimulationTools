(* Mathematica Init File *)

(* Copyright 2010-2012 Ian Hinder and Barry Wardell
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


(* Work around a bug in ImportString in Mathematica 8. Without this,
   DataRegion's definition of GetData will cause problems. *)
If[$VersionNumber == 8. && $ReleaseNumber == 0, ImportString["", "Table"]];

(* Work around a bug in Mathematica 8 where Unprotect doesn't work with
   ListLogPlot unless we call ListLogPlot once first. *)
If[$VersionNumber == 8., ListLogPlot[{1}]];

(* Give a warning to users who try to load the old nrmma package *)
If[$Input === "nrmma.m",
   Print["nrmma has been renamed to SimulationTools.  Please rename "<>$UserBaseDirectory<>"/Applications/nrmma as "<>$UserBaseDirectory<>"/Applications/SimulationTools and load the package using <<SimulationTools`"];
   Abort[]];

Get["SimulationTools`Kernel`SimulationTools`"];
Get["SimulationTools`ArgumentChecker`"];
Get["SimulationTools`Error`"];

Block[{$Path = Prepend[$Path, FileNameDrop[FindFile["SimulationTools`"], -2]<>"/PirahaPeg"]},
  Unprotect[$Packages];
  $Packages = Complement[$Packages, SimulationTools`Private`packages];
  Protect[$Packages];

  WithArgumentChecking[Scan[Needs, SimulationTools`Private`packages]];
]