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

Block[{$Path = Prepend[$Path, FileNameDrop[FindFile["SimulationTools`"], -2]<>"/PirahaPeg"]},
  Needs["SimulationTools`ArgumentChecker`"];
  Needs["SimulationTools`Error`"];

  Module[{packages =
   {"SimulationTools`Ascii1D`",
    "SimulationTools`Ascii`",
    "SimulationTools`BHCoordinates`",
    "SimulationTools`Binary`",
    "SimulationTools`BlackHole`",
    "SimulationTools`CarpetIOHDF5`",
    "SimulationTools`CarpetHDF5`",
    "SimulationTools`CoordinateTransformations`",
    "SimulationTools`Convergence`",
    "SimulationTools`DataAnalysis`",
    "SimulationTools`DataRegion`",
    "SimulationTools`DataRepresentations`",
    "SimulationTools`DataTable`",
    "SimulationTools`FieldLines`",
    "SimulationTools`GridFunctions`",
    "SimulationTools`Grids`",
    "SimulationTools`Horizons`",
    "SimulationTools`IniFile`",
    "SimulationTools`InitialData`",
    "SimulationTools`Kicks`",
    "SimulationTools`Memo`",
    "SimulationTools`MessageCatcher`",
    "SimulationTools`MinTracker`",
    "SimulationTools`Movie`",
    "SimulationTools`MultipoleASCII`",
    "SimulationTools`MultipoleHDF5`",
    "SimulationTools`NR`",
    "SimulationTools`NRDF`",
    "SimulationTools`NRExport`",
    "SimulationTools`Parameters`",
    "SimulationTools`Performance`",
    "SimulationTools`Plotting`",
    "SimulationTools`Profile`",
    "SimulationTools`Providers`",
    "SimulationTools`PunctureTracker`",
    "SimulationTools`ReadHDF5`",
    "SimulationTools`RunFiles`",
    "SimulationTools`ShiftTracker`",
    "SimulationTools`SimFactory`",
    "SimulationTools`SimView`",
    "SimulationTools`SimViewRRMHD`",
    "SimulationTools`Statistics`",
    "SimulationTools`SystemStatistics`",
    "SimulationTools`Trackers`",
    "SimulationTools`Tracks`",
    "SimulationTools`TwoPunctures`",
    "SimulationTools`Utils`",
    "SimulationTools`Waveforms`",
    "SimulationTools`YlmDecomp`",
    If[$VersionNumber >= 8, "SimulationTools`Wavelets`", Sequence[]]}},

    Unprotect[$Packages];
    $Packages = Complement[$Packages, packages];
    Protect[$Packages];

    ArgumentChecker`WithArgumentChecking[Scan[Needs, packages]];
  ]
]