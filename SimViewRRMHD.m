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

BeginPackage["SimulationTools`SimViewRRMHD`",
 {
  "SimulationTools`Ascii`",
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`Parameters`",
  "SimulationTools`Plotting`",
  "SimulationTools`RunFiles`"
 }];

SimViewRRMHD;

Begin["`Private`"];

segmentInfo[dirName_] :=
 Module[{mtFile, mtTimes, mtt1, mtt2, idNo, date, col},
  mtFile = FileNameJoin[{dirName, "/runstats.asc"}];
  col=2;
  If[FileType[mtFile] === None,
  mtFile =  FileNameJoin[{dirName, "/carpet::timing..asc"}];
  col=9];

  If[FileType[mtFile] =!= None,
   mtTimes = Flatten[ReadColumnFile[mtFile, {col}]];
   mtt1 = First[mtTimes];
   mtt2 = Last[mtTimes];
   date = DateString[FileDate[mtFile]],
   
   mtt1 = "";
   mtt2 = "";
   date = ""];
  
  idNo = StringCases[
     dirName, (__ ~~ "/output-" ~~ id__ ~~ "/" ~~ __) -> id][[1]];
  {ID -> idNo, LastDate -> date,
   T1 -> mtt1, T2 -> mtt2}]

segmentSummary[runName_] :=
 Module[{segs, segInfos, header, tab},
  segs = FindRunSegments[runName];
  segInfos = Map[segmentInfo, segs];
  header = {Style["Segment", Italic], Style["Last output", Italic], 
    Style["t1", Italic], Style["t2", Italic]};
  tab = {header}~Join~
    Map[{ID /. #, LastDate /. #, T1 /. #, T2 /. #} &, segInfos];
  Return[Grid[tab, Spacings -> 2, Alignment -> Left]];
  ]

SimViewRRMHD[runNames_(*, r_*)] :=
 Module[{EMEnergy,netcharge,divBnorm2,rhomax,alpminimum,restmass,hamnorm2,plottable,grid,size,segments,segmentSummary},
  size = {350, 100};
  size = 250;
  EMEnergy=Catch[DynamicListLinePlot[
  Map[If[Length[FindParameters[#, "whisky::whisky_Rmhd_on"]] == 1, 
     If[StringCases[LookupParameter[#, "whisky::whisky_Rmhd_on"], 
        "yes"] == {"yes"}, 
      ReadCarpetASCIIScalar[#, 
       "whisky_total_electromagnetic_energy..asc"]],
     If[Length[FindParameters[#, "whisky::whisky_mhd_on"]] == 1, 
      If[StringCases[LookupParameter[#, "whisky::whisky_mhd_on"], 
         "yes"] == {"yes"}, 
       ReadCarpetASCIIScalar[#, 
        "whisky_total_magnetic_energy..asc"]]]] &, runNames], 
  PlotRange -> All, PlotLabel -> "EM Energy", ImageSize -> size]];
  netcharge=Catch[DynamicListLinePlot[Select[Map[ToList[ReadCarpetASCIIScalar[#, 
       "total_charge..asc"]]&,runNames],# != {}&], 
  PlotRange -> All, PlotLabel -> "Net Charge", ImageSize -> size]];
  divBnorm2=Catch[DynamicListLinePlot[Select[Map[ToList[ReadCarpetASCIIScalar[#, 
       "divB.norm2.asc"]]&,runNames],# != {}&], 
  PlotRange -> All, PlotLabel -> "Norm2 of divB", ImageSize -> size]];
  rhomax = Catch[
   DynamicListLinePlot[
      Map[ReadCarpetASCIIScalar[#,"rho.maximum.asc"] &, runNames], 
    PlotRange->All, PlotLabel -> "Rho Central", 
    ImageSize -> size]];
  alpminimum = Catch[
   DynamicListLinePlot[
      Map[ReadCarpetASCIIScalar[#,"alp.minimum.asc"] &, runNames], 
    PlotRange->All, PlotLabel -> "alp minimum", 
    ImageSize -> size]];

  restmass = Catch[
   DynamicListLinePlot[
      Map[If[FindRunFile[#,"total_rest_mass_normalised..asc"]!= {},ReadCarpetASCIIScalar[#,"total_rest_mass_normalised..asc"],ReadCarpetASCIIScalar[#,"total_rest_mass_normalised.maximum.asc"]]&, runNames], 
    PlotRange->All, PlotLabel -> "Total Rest Mass", 
    ImageSize -> size]];
(*  restmass = Catch[
   DynamicListLinePlot[
      Map[ReadCarpetASCIIScalar[#,"total_rest_mass..asc"] &, runNames], 
    PlotRange->All, PlotLabel -> "Total Rest Mass", 
    ImageSize -> size]];*)
  hamnorm2 = Catch[
   DynamicListLinePlot[
      Map[ReadCarpetASCIIScalar[#,"ham.norm2.asc"] &, runNames], 
    PlotRange->All, PlotLabel -> "Norm2 of Hamiltonian constr.", 
    ImageSize -> size]];

  segments = {{Style["Simulation", Bold], Style["Segments", Bold]}}~
    Join~Map[{#, segmentSummary[#]} &, runNames];

  plottable=Partition[{EMEnergy,divBnorm2,
	   netcharge,rhomax,
       restmass,hamnorm2,alpminimum},2];

  grid = Grid[Prepend[plottable,{Text[Style[StringJoin[Riffle[runNames,", "]], Bold, 24]], SpanFromLeft}]
		~Join~segments,Spacings -> {0, 1}];
  Return[grid]
  ];

End[];

EndPackage[];
