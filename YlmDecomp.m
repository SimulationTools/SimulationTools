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

BeginPackage["SimulationTools`YlmDecomp`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataTable`",
  "SimulationTools`RunFiles`"
 }];

Begin["`Private`"];

YlmDecompPsi4Variable = "Psi4";

getFiles[runName_, l_:"*", m_:"*", r_:"*"] :=
  Module[{runFiles},
  If[FileType[runName]===File,
      runFiles = {runName},
      runFiles = Map[FileNameTake[#,-1]&, FindSimulationFiles[runName,
        "Ylm_WEYLSCAL4"~~("-"|"::")~~YlmDecompPsi4Variable<>"r_l"<>ToString[l]<>
                                         "_m"<>ToString[m]<>"_r"<>ToString[r]<>".asc"]];
  ];

  runFiles
];

SimulationTools`YlmDecomp`Waveforms`HaveData[runName_, args___] :=
  !StringMatchQ[Catch[FindRunDir[runName]], "Cannot*"] && getFiles[runName] =!= {};

SimulationTools`YlmDecomp`Waveforms`ReadPsi4Data[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  Module[{fileName, threeCols, psi4},
    fileName = "Ylm_WEYLSCAL4"~~("-"|"::")~~YlmDecompPsi4Variable<>"r_l" <>
             ToString[l] <> "_m" <> ToString[m] <> "_r" <> ToString[rad] <> ".asc";
    threeCols = ReadColumnFile[runName, fileName, {1,2,3}];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, threeCols];
    Return[MakeDataTable[psi4]]];

SimulationTools`YlmDecomp`Waveforms`ReadPsi4RadiiStrings[runName_] :=
  Module[{names, radiusFromFileName, radii},
    names = getFiles[runName];
    radiusFromFileName[name_] :=
      StringReplace[name,
        "Ylm_WEYLSCAL4"~~("-"|"::")~~YlmDecompPsi4Variable<>"r_l" ~~ __ ~~ "m" ~~ __ ~~ "r"
        ~~ x : (NumberString|"inf") ~~ ".asc" -> x];
    radii = Sort[Union[Map[radiusFromFileName, names]]]];

SimulationTools`YlmDecomp`Waveforms`ReadPsi4Modes[runName_] :=
  Module[{names},
    names = getFiles[runName];
    Sort[Round /@ ToExpression /@ (Union@@StringCases[names,
      "l" ~~ l:NumberString ~~ "_m" ~~ m:NumberString :> {l,m}])]];

End[];

EndPackage[];
