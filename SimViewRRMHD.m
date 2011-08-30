(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["SimViewRRMHD`", {"NR`","Ascii1D`","Ascii`","RunFiles`", "Plotting`","DataTable`", "Memo`", "BHCoordinates`", "SystemStatistics`","Parameters`"}];

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
 Module[{EMEnergy,netcharge,divBnorminf,rhomax,restmass,hamnorm2,grid,size,segments,segmentSummary},
  size = {350, 100};
  size = 250;
  EMEnergy=Catch[DynamicListLinePlot[
  Map[If[Length[FindParameters[#, "whisky::whisky_Rmhd_on"]] == 1, 
     If[StringCases[LookupParameter[#, "whisky::whisky_Rmhd_on"], 
        "yes"] == {"yes"}, 
      ReadCarpetASCIIFromRun[#, 
       "whisky_total_electromagnetic_energy..asc"]],
     If[Length[FindParameters[#, "whisky::whisky_mhd_on"]] == 1, 
      If[StringCases[LookupParameter[#, "whisky::whisky_mhd_on"], 
         "yes"] == {"yes"}, 
       ReadCarpetASCIIFromRun[#, 
        "whisky_total_magnetic_energy..asc"]]]] &, runNames], 
  PlotRange -> All, PlotLabel -> "EM Energy", ImageSize -> size]];
  netcharge=Catch[DynamicListLinePlot[ Select[Map[ ReadCarpetASCIIFromRun[#, 
       "total_charge..asc"]&,runNames],# != {}&], 
  PlotRange -> All, PlotLabel -> "Net Charge", ImageSize -> 250]];
  divBnorminf=Catch[DynamicListLinePlot[ Select[Map[ ReadCarpetASCIIFromRun[#, 
       "divB.norm_inf.asc"]&,runNames],# != {}&], 
  PlotRange -> All, PlotLabel -> "Infinity norm of divB", ImageSize -> 250]];
  rhomax = Catch[
   DynamicListLinePlot[
      Map[ReadCarpetASCIIFromRun[#,"rho.maximum.asc"] &, runNames], 
    PlotRange->All, PlotLabel -> "Rho Central", 
    ImageSize -> size]];
  restmass = Catch[
   DynamicListLinePlot[
      Map[If[FindRunFile[#,"total_rest_mass_normalised..asc"]!= {},ReadCarpetASCIIFromRun[#,"total_rest_mass_normalised..asc"],ReadCarpetASCIIFromRun[#,"total_rest_mass_normalised.maximum.asc"]]&, runNames], 
    PlotRange->All, PlotLabel -> "Total Rest Mass", 
    ImageSize -> size]];
(*  restmass = Catch[
   DynamicListLinePlot[
      Map[ReadCarpetASCIIFromRun[#,"total_rest_mass..asc"] &, runNames], 
    PlotRange->All, PlotLabel -> "Total Rest Mass", 
    ImageSize -> size]];*)
  hamnorm2 = Catch[
   DynamicListLinePlot[
      Map[ReadCarpetASCIIFromRun[#,"ham.norm2.asc"] &, runNames], 
    PlotRange->All, PlotLabel -> "Norm2 of Hamiltonian constr.", 
    ImageSize -> size]];

  segments = {{Style["Simulation", Bold], Style["Segments", Bold]}}~
    Join~Map[{#, segmentSummary[#]} &, runNames];

  grid = Grid[{{Text[Style[StringJoin[Riffle[runNames,", "]], Bold, 24]], SpanFromLeft},
       {EMEnergy,divBnorminf},
	   {netcharge,rhomax},
       {restmass,hamnorm2}}~Join~
       segments, 
       Spacings -> {0, 1}];
  Return[grid]
  ];

End[];

EndPackage[];
