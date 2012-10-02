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


(* Load SimulationTools packages when << SimulationTools` is run *)

(* Work around a bug in ImportString in Mathematica 8. Without this,
   DataRegion's definition of GetData will cause problems. *)
If[$VersionNumber == 8. && $ReleaseNumber == 0, ImportString["", "Table"]];

(* Work around a bug in Mathematica 8 where Unprotect doesn't work with
   ListLogPlot unless we call ListLogPlot once first. *)
If[$VersionNumber == 8., ListLogPlot[{1}]];

(* We need to modify the $Path because the packages in SimulationTools don't
   specify their dependencies as SimulationTools`DataTable`, but as just
   DataTable`.  Changing this would break existing users.  When users
   are all using SimulationTools as a Mathematica application, we can change
   this. *)

$Path = Prepend[$Path, FileNameDrop[FindFile["SimulationTools`"], -2]];
$Path = Prepend[$Path, FileNameDrop[FindFile["SimulationTools`"], -2]<>"/PirahaPeg"];

Needs["ArgumentChecker`"];

Needs["Error`"];

Module[{packages =
 {"Ascii1D",
  "Ascii",
  "BHCoordinates",
  "Binary",
  "BlackHole",
  "CarpetIOHDF5",
  "CarpetHDF5",
  "CoordinateTransformations",
  "Convergence",
  "DataAnalysis",
  "DataRegion",
  "DataRepresentations",
  "DataTable",
  "FieldLines",
  "GridFunctions",
  "Grids",
  "Horizons",
  "IniFile",
  "InitialData",
  "Kicks",
  "Memo",
  "MessageCatcher",
  "MinTracker",
  "Movie",
  "MultipoleASCII",
  "MultipoleHDF5",
  "NR",
  "NRDF",
  "NRExport",
  "Parameters",
  "Performance",
  "Piraha",
  "Plotting",
  "Profile",
  "Providers",
  "PunctureTracker",
  "ReadHDF5",
  "RunFiles",
  "ShiftTracker",
  "SimFactory",
  "SimView",
  "SimViewRRMHD",
  "Statistics",
  "SystemStatistics",
  "Trackers",
  "Tracks",
  "TwoPunctures",
  "Utils",
  "Waveforms",
  "YlmDecomp",
  If[$VersionNumber >= 8, "Wavelets", Sequence[]]}},

  packages = Map[#<>"`"&, packages];
  Unprotect[$Packages];
  $Packages = Complement[$Packages, packages];
  Protect[$Packages];

  ArgumentChecker`WithArgumentChecking[Scan[Needs, packages]];

  SimulationToolsDoc[] :=
    Scan[Information[(# ~~ (Except["`"] ..)),
       LongForm -> False] &, packages];
]
