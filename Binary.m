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

BeginPackage["Binary`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "RunFiles`",
  "Trackers`",
  "Horizons`"
 }];

ReadBinaryCoordinates::usage = "ReadBinaryCoordinates[run, i] gives the Cartesian coordinates of body i of the binary as a function of time.  The result is a list of DataTables, one for each coordinate direction.\nReadBinaryCoordinates[run] gives the Cartesian coordinates of the relative orbit of the binary.";

ReadBinaryVelocity::usage = "ReadBinaryVelocity[run, i] gives the Cartesian components of the velocity of body i of the binary as a function of time.  The result is a list of DataTables, one for each coordinate direction. \nReadBinaryVelocity[run] gives the Cartesian components of the velocity of the relative orbit of the binary.";

ReadBinaryRadius::usage = "ReadBinaryRadius[run, i] gives the spherical polar radial coordinate of body i of the binary as a function of time as a DataTable.\nReadBinaryRadius[run] gives the separation of the binary.";

ReadBinaryAzimuth::usage = "ReadBinaryAzimuth[run, i] gives the spherical polar azimuthal coordinate (phi) of body i of the binary as a function of time as a DataTable.\nReadBinaryAzimuth[run] gives the azimuthal coordinate of the relative orbit of the binary.";

ReadBinaryInclination::usage = "ReadBinaryInclination[run] gives the spherical polar inclination coordinate (theta) of body i of the binary as a function of time as a DataTable.\nReadBinaryInclination[run] gives the inclination angle of the relative orbit of the binary.";

ReadBinarySeparation::usage = "ReadBinarySeparation[run] gives the distance between the two bodies in the binary as a function of time as a DataTable.";
ReadBinaryPhase::usage = "ReadBinaryPhase[run] gives the phase, phi, (in the xy plane) of the binary's relative orbit.";

ToListOfPoints::usage = "ToListOfPoints[{x, y, ...}] takes DataTables representing x, y, ... coordinates and gives a list {{x1, y1, ...}, {x2, y2, ...}, ...} suitable for plotting with Line or ListPointPlot3D.";

Begin["`Private`"];

(* We assume that there is only one binary in a simulation *)
Options[binaryTracker] = {"Trackers" -> Automatic};
binaryTracker[run_String, i:(1|2), opts:OptionsPattern[]] :=
  Which[
    OptionValue[Trackers] =!= Automatic,
    If[!MatchQ[OptionValue[Trackers], {{_String, _Integer}, {_String, _Integer}}],
       Error["Unrecognised Trackers option: should be {{trackertype1, index1}, {trackertype2, index2}}"]];
    OptionValue[Trackers][[i]],

    PunctureTracker`BHCoordinates`HaveData[run, 0] &&
    PunctureTracker`BHCoordinates`HaveData[run, 1],
    {"PunctureTracker", i-1},
    
    HaveHorizonData[run, 1] &&
    HaveHorizonData[run, 2],
    {"Horizons", i},
    
    MinTracker`BHCoordinates`HaveData[run, 0] &&
    MinTracker`BHCoordinates`HaveData[run, 1],
    {"MinTracker", i-1},

    ShiftTracker`BHCoordinates`HaveData[run, 0] &&
    ShiftTracker`BHCoordinates`HaveData[run, 1],
    {"ShiftTracker", i-1},

    True,
    Error["No binary data (PunctureTracker, AHFinderDirect, MinTracker or ShiftTracker) found in run "<>run]];

(* Map the binary functions to the tracker functions, determining the
   correct tracker to use automatically. *)

Do[
  Module[
    {binaryFn = fn[[1]],
     trackerFn = fn[[2]]},

Options[binaryFn] = Options[binaryTracker];
    
Evaluate[binaryFn][run_, i_, opts:OptionsPattern[]] :=
    Evaluate[trackerFn][run, binaryTracker[run,i,opts]];

Evaluate[binaryFn][run_, opts:OptionsPattern[]] :=
    Evaluate[trackerFn][run, Sequence@@Table[binaryTracker[run,i,opts],{i,1,2}]]],

  {fn, {ReadBinaryCoordinates -> ReadTrackerCoordinates,
        ReadBinaryVelocity -> ReadTrackerVelocity,
        ReadBinaryRadius -> ReadTrackerRadius,
        ReadBinaryAzimuth -> ReadTrackerAzimuth,
        ReadBinaryInclination -> ReadTrackerInclination}}];

Options[ReadBinarySeparation] = Options[binaryTracker];
ReadBinarySeparation[run_, opts:OptionsPattern[]] :=
  ReadBinaryRadius[run,opts];

Options[ReadBinaryPhase] = Options[binaryTracker];
ReadBinaryPhase[run_, opts:OptionsPattern[]] :=
  ReadBinaryAzimuth[run,opts];

ToListOfPoints[ds:{_DataTable...}] :=
  Transpose[ToListOfData/@ds];

End[];
EndPackage[];
