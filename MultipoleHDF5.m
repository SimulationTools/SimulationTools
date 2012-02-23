
BeginPackage["MultipoleHDF5`", {"RunFiles`", "DataTable`", "Memo`", "ReadHDF5`", "Error`"}];

Begin["`Private`"];

MultipolePsi4Variable = "psi4";

getFiles[runName_] :=
  Module[{runFiles},
  If[FileType[runName]===File,
      runFiles = {runName},
      runFiles = FindRunFile[runName, "mp_"<>MultipolePsi4Variable<>".h5"];
  ];

  runFiles
];

MultipoleHDF5`Waveforms`HaveData[runName_, args___] :=
  HaveRunDir[runName] && MultipoleHDF5`Waveforms`ReadPsi4RadiiStrings[runName] =!= {};

MultipoleHDF5`Waveforms`ReadPsi4Data[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  Module[{datasetName, runFiles, files, data, psi4},
    runFiles = getFiles[runName];
    datasetName = "l" <> ToString[l] <> "_m" <> ToString[m] <> "_r" <> ToString[rad];
    files = Map[ReadHDF5[#,{"Datasets", datasetName}] &, runFiles];
    data = MergeFiles[files];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, data];
    Return[MakeDataTable[psi4]]];

MultipoleHDF5`Waveforms`ReadPsi4RadiiStrings[runName_] :=
  Module[{datasets, radii},
    datasets = Union@@Map[ReadHDF5[#] &, getFiles[runName]];
    radii = Sort[(Union@@StringCases[datasets, "r" ~~ x : (NumberString|"inf") :> x])];
    radii
  ];

MultipoleHDF5`Waveforms`ReadPsi4Modes[runName_] :=
  Module[{datasets, modes},
    datasets = Union@@Map[ReadHDF5, getFiles[runName]];
    modes = Sort[Round /@ ToExpression /@ (Union@@StringCases[datasets, "l" ~~ l:NumberString ~~ "_m" ~~ m:NumberString :> {l,m}])];
    modes];

End[];

EndPackage[];
