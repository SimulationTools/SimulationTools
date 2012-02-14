
BeginPackage["MultipoleASCII`", {"RunFiles`", "DataTable`", "Memo`"}];

Begin["`Private`"];

MultipolePsi4Variable = "psi4";

getFiles[runName_, l_:"*", m_:"*", r_:"*"] :=
  Module[{runFiles},
   If[FileType[runName]===File,
      runFiles = {runName},
      runFiles = FindRunFilesFromPattern[runName,
        "mp_"<>MultipolePsi4Variable<>"_l"<>ToString[l]<>"_m"<>ToString[m]<>"_r"<>ToString[r]<>".asc"]];

  runFiles
];

MultipoleASCII`Waveforms`HaveData[runName_, args___] :=
  !StringMatchQ[Catch[FindRunDir[runName]], "Cannot*"] && MultipoleASCII`Waveforms`ReadPsi4RadiiStrings[runName] =!= {};

MultipoleASCII`Waveforms`ReadPsi4Data[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  Module[{fileName, threeCols, psi4},
    fileName = "mp_"<>MultipolePsi4Variable<>"_l" <>
             ToString[l] <> "_m" <> ToString[m] <> "_r" <> ToString[rad] <> ".asc";
    threeCols = ReadColumnFile[runName, fileName, {1,2,3}];
    psi4 = Map[{#[[1]], #[[2]] + I #[[3]]}&, threeCols];
    Return[MakeDataTable[psi4]]];

MultipoleASCII`Waveforms`ReadPsi4RadiiStrings[runName_] :=
  Module[{names, radiusFromFileName, radii},
    names = getFiles[runName];
    radiusFromFileName[name_] :=
      StringReplace[name,
        "mp_"<>MultipolePsi4Variable<>"_l" ~~ __ ~~ "m" ~~ __ ~~ "r"
        ~~ x : (NumberString|"inf") ~~ ".asc" -> x];
    radii = Sort[Union[Map[radiusFromFileName, names]]]];

MultipoleASCII`Waveforms`ReadPsi4Modes[runName_] :=
  Module[{names},
   names = getFiles[runName];
   Sort[Round /@ ToExpression /@ 
                (Union@@StringCases[names, "l" ~~ l:NumberString ~~
                                    "_m" ~~ m:NumberString :> {l,m}])]];

End[];

EndPackage[];
