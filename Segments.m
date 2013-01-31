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

BeginPackage["SimulationTools`Segments`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`Profile`",
  "SimulationTools`Providers`",
  "SimulationTools`RunFiles`",
  "SimulationTools`"
 }];

SegmentStartDate;
SegmentEndDate;
SegmentDuration;
RunDutyCycle;
SegmentCoordinateTimeInterval;
SegmentStartTimes;

Begin["`Private`"];

SegmentStartDate[dir_] :=
  FileDate[FileNameJoin[{dir, "/carpet::timing..asc"}]];

SegmentEndDate[dir_] :=
  Module[{parFile},
    parFile = First[FileNames["*.par", dir]];
    FileDate[parFile]];

SegmentDuration[dir_] :=
  DateDifference[SegmentEndDate[dir], SegmentStartDate[dir], "Second"][[1]];

RunDutyCycle[run_] :=
  Module[{segs = FindRunSegments[run], totalRunTime, totalElapsedTime},
    totalRunTime = Plus@@(SegmentDuration /@ segs);
    totalElapsedTime = DateDifference[SegmentStartDate[First[segs]],
                                      SegmentEndDate[Last[segs]], "Second"][[1]];
    totalRunTime / totalElapsedTime //N];

SegmentCoordinateTimeInterval[dir_] :=
 Module[{times =
    Catch[First /@ ReadColumnFile[dir, "carpet::timing..asc", ColumnNumbers[{"time"}]]]},
  If[! ListQ[times], Return[None], Return[{times[[1]], times[[-1]]}]]];

SegmentStartTimes[run_] :=
 Module[{segs = FindRunSegments[run]},
  First /@
   Select[SegmentCoordinateTimeInterval /@ segs, # =!= None &]];

End[];
EndPackage[];
