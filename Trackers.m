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

BeginPackage["Trackers`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "RunFiles`"
 }];

ReadTrackerCoordinates::usage = "ReadTrackerCoordinates[run, tracker] gives the Cartesian coordinates of the tracker as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker is a list of the form {type, index}.\nReadTrackerCoordinates[run, tracker1, tracker2] gives the Cartesian components of the vector joining the two trackers as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker1 and tracker2 are lists of the form {type, index}.";
ReadTrackerVelocity::usage = "ReadTrackerVelocity[run, tracker] gives a list of the Cartesian components of the velocity of the tracker as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker is a list of the form {type, index}.\nReadTrackerVelocity[run, tracker1, tracker2] gives a list of the Cartesian components of the relative velocity of the two trackers as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker1 and tracker2 are lists of the form {type, index}.";
ReadTrackerRadius::usage = "ReadTrackerRadius[run, tracker] gives the spherical polar radial coordinate of a tracker as a DataTable as a function of time.\nReadTrackerRadius[run, tracker1, tracker2] gives the distance between the two trackers.";
ReadTrackerAzimuth::usage = "ReadTrackerAzimuth[run, tracker] gives the spherical polar azimuthal coordinate (phi) of a tracker as a DataTable as a function of time.\nReadTrackerAzimuth[run, tracker1, tracker2] gives the azimuthal angle of the vector joining the two trackers.";
ReadTrackerInclination::usage = "ReadTrackerInclination[run, tracker] gives the spherical polar inclination coordinate (theta) of a tracker as a DataTable as a function of time.\nReadTrackerInclination[run, tracker1, tracker2] gives the inclination angle of the vector joining the two trackers.";

Begin["`Private`"];

trackerPattern = {_String, _Integer};

ReadTrackerCoordinates[run_String, tracker:{type_, i_}] :=
  Symbol[type<>"`Trackers`ReadCoordinates"][run, i];

ReadTrackerCoordinates[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  ReadTrackerCoordinates[run, tracker1] - ReadTrackerCoordinates[run, tracker2];

ReadTrackerVelocity[run_String, tracker:trackerPattern] :=
  NDerivative[1]/@ReadTrackerCoordinates[run, tracker];

ReadTrackerVelocity[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  NDerivative[1]/@ReadTrackerCoordinates[run, tracker1, tracker2];

ReadTrackerRadius[run_String, tracker:trackerPattern] :=
  Norm@ReadTrackerCoordinates[run, tracker];

ReadTrackerRadius[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  Norm@ReadTrackerCoordinates[run, tracker1, tracker2];

xyToAzimuth[{x1_DataTable, y1_DataTable}] :=
  Module[{x,y,t},
    {x,y} = ToListOfData /@ IntersectDataTables[{x1,y1}];
    t = ToListOfCoordinates[x1];
    ToDataTable@DataRepresentations`Private`phase[MapThread[{#1,{#2,#3}} &,{t,x,y}]]];

ReadTrackerAzimuth[run_String, tracker:trackerPattern] :=
  xyToAzimuth[ReadTrackerCoordinates[run, tracker][[{1,2}]]];

ReadTrackerAzimuth[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  xyToAzimuth[(ReadTrackerCoordinates[run, tracker1] - ReadTrackerCoordinates[run, tracker2])[[{1,2}]]];

ReadTrackerInclination[run_String, tracker:trackerPattern] :=
  Module[{R,x,y,z},
    R = {x,y,z} = ReadTrackerCoordinates[run, tracker];
    (* In BHCoordinates, we avoided infinities at the origin:
       MapData[If[Norm[#] > 10^-4, ArcCos[#[[3]]/Norm[#]], 0.0] &, rp]; *)
    ArcCos[z]/Norm[R]];

ReadTrackerInclination[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  Module[{R,x,y,z},
    R = {x,y,z} = ReadTrackerCoordinates[run, tracker1] - ReadTrackerCoordinates[run, tracker2];
    (* In BHCoordinates, we avoided infinities at the origin:
       MapData[If[Norm[#] > 10^-4, ArcCos[#[[3]]/Norm[#]], 0.0] &, rp]; *)
    ArcCos[z]/Norm[R]];

End[];
EndPackage[];
