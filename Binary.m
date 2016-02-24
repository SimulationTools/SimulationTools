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

BeginPackage["SimulationTools`Binary`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Plotting`",
  "SimulationTools`Trackers`",
  "SimulationTools`Horizons`"
 }];

ReadBinaryCoordinates::usage = "ReadBinaryCoordinates[sim, i] gives the Cartesian coordinates of body i of the binary as a function of time.  The result is a list of DataTables, one for each coordinate direction.\nReadBinaryCoordinates[sim] gives the Cartesian coordinates of the relative orbit of the binary.";

ReadBinaryVelocity::usage = "ReadBinaryVelocity[sim, i] gives the Cartesian components of the velocity of body i of the binary as a function of time.  The result is a list of DataTables, one for each coordinate direction. \nReadBinaryVelocity[sim] gives the Cartesian components of the velocity of the relative orbit of the binary.";

ReadBinaryRadius::usage = "ReadBinaryRadius[sim, i] gives the spherical polar radial coordinate of body i of the binary as a function of time as a DataTable.\nReadBinaryRadius[sim] gives the separation of the binary.";

ReadBinaryAzimuth::usage = "ReadBinaryAzimuth[sim, i] gives the spherical polar azimuthal coordinate (phi) of body i of the binary as a function of time as a DataTable.\nReadBinaryAzimuth[sim] gives the azimuthal coordinate of the relative orbit of the binary.";

ReadBinaryInclination::usage = "ReadBinaryInclination[sim] gives the spherical polar inclination coordinate (theta) of body i of the binary as a function of time as a DataTable.\nReadBinaryInclination[sim] gives the inclination angle of the relative orbit of the binary.";

ReadBinarySeparation::usage = "ReadBinarySeparation[sim] gives the distance between the two bodies in the binary as a function of time as a DataTable.";
ReadBinaryPhase::usage = "ReadBinaryPhase[sim] gives the phase, phi, (in the xy plane) of the binary's relative orbit.";

ToListOfPoints::usage = "ToListOfPoints[{x, y, ...}] takes DataTables representing x, y, ... coordinates and gives a list {{x1, y1, ...}, {x2, y2, ...}, ...} suitable for plotting with Line or ListPointPlot3D.";

Begin["`Private`"];

Options[binaryTracker] = {"Trackers" -> Automatic};
binaryTracker[run_String, i:(1|2|{1,2}), opts:OptionsPattern[]] :=
  Module[
    {b},
    b = binaryTrackerNoFail[run,i,opts];
    If[b === None, 
       Error["No binary data (PunctureTracker, AHFinderDirect, MinTracker or ShiftTracker) found in run "<>run],
       b]];

(* We assume that there is only one binary in a simulation *)
Options[binaryTrackerNoFail] = Options[binaryTracker];
binaryTrackerNoFail[run_String, i:(1|2|{1,2}), opts:OptionsPattern[]] :=
  Which[
    OptionValue[Trackers] =!= Automatic,
    If[!MatchQ[OptionValue[Trackers], {{_String, _Integer}, {_String, _Integer}}],
       Error["Unrecognised Trackers option: should be {{trackertype1, index1}, {trackertype2, index2}}"]];
    OptionValue[Trackers][[i]],

    SimulationTools`PunctureTracker`BHCoordinates`HaveData[run, 0] &&
    SimulationTools`PunctureTracker`BHCoordinates`HaveData[run, 1],
    {"PunctureTracker", i-1},
    
    HaveHorizonData[run, 1] &&
    HaveHorizonData[run, 2],
    {"Horizons", i},
    
    SimulationTools`MinTracker`BHCoordinates`HaveData[run, 0] &&
    SimulationTools`MinTracker`BHCoordinates`HaveData[run, 1],
    {"MinTracker", i-1},

    SimulationTools`ShiftTracker`BHCoordinates`HaveData[run, 0] &&
    SimulationTools`ShiftTracker`BHCoordinates`HaveData[run, 1],
    {"ShiftTracker", i-1},

    SimulationTools`NRDF`BHCoordinates`HaveData[run, 1] &&
    SimulationTools`NRDF`BHCoordinates`HaveData[run, 2],
    {"NRDF", i},

    True,
    None];


(* Map the binary functions to the tracker functions, determining the
   correct tracker to use automatically. *)

Do[
  Module[
    {binaryFn = fn[[1]],
     trackerFn = fn[[2]]},

Options[binaryFn] = Options[binaryTracker];
    
Evaluate[binaryFn][run_, i_, opts:OptionsPattern[]] :=
    Evaluate[trackerFn][run, binaryTracker[run,i,opts]];

DocumentationBuilder`SymbolDescription["ReadBinaryCoordinates"] =
  "read coordinate locations of members of a binary system";

Evaluate[binaryFn][run_, opts:OptionsPattern[]] :=
    Evaluate[trackerFn][run, binaryTracker[run,1,opts], binaryTracker[run,2,opts]]],

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

tracks2D[{{x1_,y1_,z1_},{x2_,y2_,z2_}}] :=
  {ToListOfPoints[{x1,y1}], ToListOfPoints[{x2,y2}]};

xyToAzimuth = SimulationTools`Trackers`Private`xyToAzimuth;
inclination = SimulationTools`Trackers`Private`inclination;

SimulationTools`Binary`SimulationOverview`Plots[runNames1_] :=
  Module[{runNames, coords, plots},
    runNames = Select[runNames1, (binaryTrackerNoFail[#,1] =!= None && binaryTrackerNoFail[#,2] =!= None) &];
    coords = Table[ReadBinaryCoordinates[run, {1,2}], {run, runNames}];
    plots = If[runNames === {},
       None,
       {{PresentationListLinePlot[
         Flatten[tracks2D/@coords,1],
           AspectRatio -> Automatic, PlotLabel -> "Trajectories", PlotRange -> All]},
        {PresentationListLinePlot[
          Map[Norm[Subtract @@ #]&, coords],
          PlotRange -> {0, All}, PlotLabel -> "Separation",
          PlotLegend -> runNames, LegendPosition -> {Right, Top}],
         PresentationListLinePlot[
           Map[NDerivative[xyToAzimuth[Subtract @@ #]]&, coords[[All, All, {1,2}]]],
           PlotRange -> {0, Automatic}, PlotLabel -> "Frequency"]}}];
    plots
];

End[];
EndPackage[];
