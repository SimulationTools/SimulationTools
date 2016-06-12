Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

TestReferenceDirectory = FileNameJoin[{$SimulationToolsInstallationDirectory,"Data/TestReference"}];

testDirectory = FileNameJoin[{$TemporaryDirectory,"SimulationTools-unit-tests","Waveforms"}];
DeleteDirectory[testDirectory,DeleteContents->True];
CreateDirectory[testDirectory];

(****************************************************************)
(* WaveformCycles                                               *)
(****************************************************************)

VerificationTest[
    ReadWaveformCycles[$SimulationToolsTestSimulation, 150]
    ,
    2.7650708168336315
    ,
    TestID->"ReadWaveformCycles",
	SameTest -> ((Abs[#1-#2] < 6*10^-5) &)
]

(****************************************************************)
(* WaveformCycles                                               *)
(****************************************************************)

VerificationTest[
    WaveformCycles[ReadPsi4[$SimulationToolsTestSimulation,2,2,100], 150]
    ,
    2.7650708168336315
    ,
    TestID->"WaveformCycles",
	SameTest -> ((Abs[#1-#2] < 6*10^-5) &)
]


(******************************* Waveforms *********************************)

Module[
	{run=FileNameJoin[{$TemporaryDirectory,"wavesim_test_mp_asc"}],
	data=Table[N@{t, Exp[-(t - 15)^2/4^2] Exp[I t]}, {t, 0, 10 Pi, 2 Pi/20}]},
	Quiet[CreateDirectory[run], CreateDirectory::filex];
	Export[FileNameJoin[{run,"mp_psi4_l2_m2_r30.00.asc"}],Map[{#[[1]],Re[#[[2]]],Im[#[[2]]]} &, data],"Table"];
	VerificationTest[
		ReadPsi4[run,2,2,30]
	,
		DataTable[Transpose[data],RunName -> run]
	,
	TestID->"ReadPsi4_mp_asc"]]


VerificationTest[
  ToList[ReadPsi4["nan",2,2,100]]
  ,
  {{0., Indeterminate}, {0., 0. + 0. I}}
  ,
  TestID->"ReadPsi4 with NaNs"
    ]

VerificationTest[
  ToList[ReadPsi4[$SimulationToolsTestSimulation,2,2,100]][[{1,2,200,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadPsi4.m"}]]
  ,
  TestID->"ReadPsi4"
    ]

VerificationTest[
  WithExceptions[ReadPsi4[$SimulationToolsTestSimulation,2,2,99], Psi4RadiusNotFound -> $Failed]
  ,
  $Failed
  ,
  TestID->"ReadPsi4-2"
    ]

VerificationTest[
  MatchQ[WithExceptions[ReadPsi4[$SimulationToolsTestSimulation,2,2,100.001], Psi4RadiusNotFound -> $Failed], _DataTable]
  ,
  True
  ,
  TestID->"ReadPsi4-3"
    ]

VerificationTest[
  ReadPsi4Radii[$SimulationToolsTestSimulation]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadPsi4Radii.m"}]]
  ,
  TestID->"ReadPsi4Radii"
    ]

VerificationTest[
  ExtrapolateScalar[2,Table[{r, 20 + r^-1 + 2 r^-2}, {r, 10, 30, 5}]]
  ,
  19.99999999999999
  ,
  SameTest -> withinRoundoff,
  TestID->"ExtrapolateScalar-1"
    ]

VerificationTest[
  ExtrapolateScalar[1,Table[{r, 20 + r^-1 + 2 r^-2}, {r, 1, 5, 1}]]
  ,
  19.46294218585005
  ,
  SameTest -> withinRoundoff,
  TestID->"ExtrapolateScalar-2"
    ]

VerificationTest[
  ToList@ExtrapolateDataTables[1, Table[{r,MakeDataTable[Table[{t, t + 20 + r^-1 + 2 r^-2},
                                                               {t, 0, 10, 5}]]}, 
                                        {r, 1, 5, 1}]]
  ,
  {{0, 19.46294218585005}, {5, 24.46294218585005}, {10, 29.462942185850054}}
  ,
  SameTest -> withinRoundoff,
  TestID->"ExtrapolateDataTables-1"
    ]

VerificationTest[
  ToList@ExtrapolateDataTables[1, Table[{r,MakeDataTable[Table[{t, t + 20 + r^-1 + 2 r^-2},
                                                               {t, 0, 10, 5}]]}, 
                                        {r, 1, 5, 1}],{2,4}]
  ,
  {{0, 19.73214285714284}, {5, 24.732142857142843}, {10, 29.73214285714284}}
  ,
  SameTest -> withinRoundoff,
  TestID->"ExtrapolateDataTables-2"
    ]

(****************************************************************)
(* RadialExtrapolation                                          *)
(****************************************************************)

VerificationTest[
  RadialExtrapolation[Transpose@Table[{r, 20 + r^-1 + 2 r^-2}, {r, 10, 30, 5}],
                      2]
  ,
  19.99999999999999
  ,
  SameTest -> withinRoundoff,
  TestID->"RadialExtrapolation-1"
    ]

VerificationTest[
  RadialExtrapolation[Transpose@Table[{r, 20 + r^-1 + 2 r^-2}, {r, 1, 5, 1}], 
                      1]
  ,
  19.46294218585005
  ,
  SameTest -> withinRoundoff,
  TestID->"RadialExtrapolation-2"
    ]

VerificationTest[
  ToList@RadialExtrapolation[Transpose@Table[
    {r, ToDataTable[Table[{t, t + 20 + r^-1 + 2 r^-2},
                       {t, 0, 10, 5}]]}, 
    {r, 1, 5, 1}], 1]
  ,
  {{0, 19.46294218585005}, {5, 24.46294218585005}, {10, 29.462942185850054}}
  ,
  SameTest -> withinRoundoff,
  TestID->"RadialExtrapolationDataTable-1"
    ]

(****************************************************************)
(* AlignPhases                                                  *)
(****************************************************************)

VerificationTest[
  Map[ToList, 
      AlignPhases[
        Table[MakeDataTable[
          Table[{t, dph + (t - 5)^2}, {t, 0, 10, 1}]], {dph, {0, 2 Pi, 0.1,
                                                              2 Pi + 0.1, 2 Pi - 0.1}}], 5]][[All, 6]],
  {{5, 0.}, {5, 0.}, {5, 0.1}, {5, 0.09999999999999964}, {5, -0.09999999999999964}}
  ,
  TestID->"AlignPhases"
    ]

(****************************************************************)
(* TortoiseCoordinate                                           *)
(****************************************************************)

VerificationTest[
  TortoiseCoordinate[r, Madm],
  r + 2 Madm Log[r/(2. Madm) - 1.],
  TestID->"TortoiseCoordinate"
    ]

(****************************************************************)
(* ExtrapolateRadiatedQuantity                                  *)
(****************************************************************)

VerificationTest[
ToList[ExtrapolateRadiatedQuantity[
  Table[{r, 
    MakeDataTable[
     Table[{t, t - r + 20 + r^-1 + 2 r^-2}, {t, 0, 100, 5}]]}, {r, 2, 
    50, 1}], UseTortoiseCoordinate -> False, ExtrapolationOrder -> 2]],
  {{-2, 18.000000000000007}, {3, 23.00000000000001}, {8, 28.00000000000001}, {13, 33.00000000000002}, 
   {18, 38.000000000000014}, {23, 43.00000000000001}, {28, 48.000000000000014}, {33, 53.000000000000014}, 
   {38, 58.00000000000004}, {43, 63.000000000000036}, {48, 68.00000000000001}},
  SameTest -> withinRoundoff,
  TestID->"ExtrapolateRadiatedQuantity"
    ]

(****************************************************************)
(* RadiallyExtrapolatedWave                                     *)
(****************************************************************)

VerificationTest[
ToList[RadiallyExtrapolatedWave[
  Transpose@Table[{r, 
    MakeDataTable[
     Table[{t, t - r + 20 + r^-1 + 2 r^-2}, {t, 0, 100, 5}]]}, {r, 2, 
    50, 1}], 2, AbsPhase -> False]],
  {{-2, 18.000000000000007}, {3, 23.00000000000001}, {8, 28.00000000000001}, {13, 33.00000000000002}, 
   {18, 38.000000000000014}, {23, 43.00000000000001}, {28, 48.000000000000014}, {33, 53.000000000000014}, 
   {38, 58.00000000000004}, {43, 63.000000000000036}, {48, 68.00000000000001}},
  SameTest -> withinRoundoff,
  TestID->"RadiallyExtrapolatedWave"
    ]

(****************************************************************)
(* ToAbsPhase *)
(****************************************************************)

VerificationTest[ToAbsPhase[ToDataTable[{{0.0, 0.+I},{0.1, 1. + I}}]],
     {ToDataTable[{{0.0,1.},{0.1,Sqrt[2]//N}}],
      ToDataTable[{{0.0,Pi/2.},{0.1,Pi/4.}}]},
     TestID->"ToAbsPhase"]

(****************************************************************)
(* ToComplex *)
(****************************************************************)

VerificationTest[ToComplex[{ToDataTable[{{0.0,1.},{0.1,Sqrt[2]//N}}],
                ToDataTable[{{0.0,Pi/2.},{0.1,Pi/4.}}]}],
     ToDataTable[{{0.0, 0.+I},{0.1, 1. + I}}],
     SameTest -> ((Abs[GridNorm[#1-#2]] < 10.^-16) &),
     TestID->"ToComplex"]

(****************************************************************)
(* ToRetardedTime *)
(****************************************************************)

VerificationTest[ToRetardedTime[30., ToDataTable[{{0.0,1.},{1.0,2.0}}]],
     ToDataTable[{{-30., 1.},{-29.,2.0}}],
    TestID -> "ToRetardedTime-1"]     

VerificationTest[ToRetardedTime[30., ToDataTable[{{0.0, 1.}, {2.0, 2.0}}],
                    Function[r,r^2]],
     ToDataTable[{{0.0-30.^2, 1.},{2.-30.^2.,2.0}}],
    TestID -> "ToRetardedTime-2"]

(****************************************************************)
(* ReadRadiallyExtrapolatedPsi4 *)
(****************************************************************)

(* These no longer give the same answer by default because the
   resampling behaviour has changed.  We now resample onto the grid of
   the highest resolution version instead of constructing a new
   uniform grid.  This preserves nonuniform DataTables. Test that the
   results are the same when the compatibility variable is set *)
ReadRadiallyExtrapolatedPsi4SameQ[new_,old_] := (withinRoundoff[Norm[ToListOfCoordinates[new]], Norm[ToListOfCoordinates[old]]] && withinRoundoff[GridNorm[new],GridNorm[old]]);

VerificationTest[
   Block[{$UniformGridExtrapolation = True},  ReadRadiallyExtrapolatedPsi4[
    $SimulationToolsTestSimulation,2,2,1,
    RadialCoordinateTransformation->RadialToTortoise]],
  ExtrapolatePsi4[$SimulationToolsTestSimulation, 2, 2,
                  ExtrapolationOrder -> 1],
  SameTest -> ReadRadiallyExtrapolatedPsi4SameQ,
  TestID -> "ReadRadiallyExtrapolatedPsi4"]

(****************************************************************)
(* StrainFromPsi4 *)
(****************************************************************)

Block[{Internal`$SameQTolerance = 5},
VerificationTest[
  Psi4ToStrain[ToDataTable[{{0.0,1+3I},{0.1,5+0.2I}}],0.1]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"StrainFromPsi4-1.m"}]]
  ,
  TestID->"Psi4ToStrain-1"
    ]

VerificationTest[
  ToList[Psi4ToStrain[ReadPsi4[$SimulationToolsTestSimulation,2,2,100],0.02]][[{1,2,200,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"StrainFromPsi4-2.m"}]]
  ,
  TestID->"Psi4ToStrain-2"
    ]
]

(****************************************************************)
(* ImportWaveform and ExportWaveform *)
(****************************************************************)

Module[
  {filename = FileNameJoin[{testDirectory,"ExportWaveform.asc"}],
   waveform = ToDataTable@Table[{t, Exp[-(t - 300)^2/100^2] Exp[I 0.1 t]}, {t, 0, 600, 10}]},
  VerificationTest[

  ExportWaveform[filename, waveform];
  ImportWaveform[filename]
  ,
  waveform
  ,
  TestID->"ImportExportWaveform"
    ]]
