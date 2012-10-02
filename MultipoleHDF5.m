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

BeginPackage["SimulationTools`MultipoleHDF5`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`ReadHDF5`",
  "SimulationTools`RunFiles`"
 }];

$MultipolePsi4Variable::usage = "$MultipolePsi4Variable specifies the name of the "<>
 "variable used to store Psi4 by the Multipole thorn.";

Begin["`Private`"];

$MultipolePsi4Variable = "psi4";
DocumentationBuilder`MoreInformation["$MultipolePsi4Variable"] =
 {
  "The value of $MultipolePsi4Variable should be the same as is set in the 'name' "<>
  "section of the 'variable' parameter for the Multipole thorn."
 };

getFiles[runName_] :=
  Module[{runFiles},
  If[FileType[runName]===File,
      runFiles = {runName},
      runFiles = FindRunFile[runName, "mp_"<>$MultipolePsi4Variable<>".h5"];
      If[runFiles === {},
        runFiles = FindRunFile[runName, $MultipolePsi4Variable<>".h5"];
      ];
  ];

  runFiles
];

SimulationTools`MultipoleHDF5`Waveforms`HaveData[runName_, args___] :=
  HaveRunDir[runName] && SimulationTools`MultipoleHDF5`Waveforms`ReadPsi4RadiiStrings[runName] =!= {};

SimulationTools`MultipoleHDF5`Waveforms`ReadPsi4Data[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  Module[{datasetName, runFiles, files, data, psi4},
    runFiles = getFiles[runName];
    datasetName = "l" <> ToString[l] <> "_m" <> ToString[m] <> "_r" <> ToString[rad];
    files = Map[ReadHDF5[#,{"Datasets", datasetName}] &, runFiles];
    data = MergeFiles[files];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, data];
    Return[MakeDataTable[psi4]]];

SimulationTools`MultipoleHDF5`Waveforms`ReadPsi4RadiiStrings[runName_] :=
  Module[{datasets, radii},
    datasets = Union@@Map[ReadHDF5[#] &, getFiles[runName]];
    radii = Sort[(Union@@StringCases[datasets, "r" ~~ x : (NumberString|"inf") :> x])];
    radii
  ];

SimulationTools`MultipoleHDF5`Waveforms`ReadPsi4Modes[runName_] :=
  Module[{datasets, modes},
    datasets = Union@@Map[ReadHDF5, getFiles[runName]];
    modes = Sort[Round /@ ToExpression /@ (Union@@StringCases[datasets, "l" ~~ l:NumberString ~~ "_m" ~~ m:NumberString :> {l,m}])];
    modes];

End[];

EndPackage[];
