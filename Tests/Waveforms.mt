(* Mathematica Test File *)

$SimulationPath = {$NRMMATestSimulationDirectory};

TestReferenceDirectory = FileNameJoin[{FileNameDrop[FindFile["nrmma`"],-2],"Data/TestReference"}];


(******************************* Waveforms *********************************)

Module[
	{run=FileNameJoin[{$TemporaryDirectory,"wavesim_test_mp_asc"}],
	data=Table[N@{t, Exp[-(t - 15)^2/4^2] Exp[I t]}, {t, 0, 10 Pi, 2 Pi/20}]},
	Quiet[CreateDirectory[run], CreateDirectory::filex];
	Export[FileNameJoin[{run,"mp_psi4_l2_m2_r30.00.asc"}],Map[{#[[1]],Re[#[[2]]],Im[#[[2]]]} &, data],"Table"];
	Test[
		ReadPsi4[run,2,2,30]
	,
		DataTable[data,RunName -> run]
	,
	TestID->"ReadPsi4_mp_asc"]]

Test[
  ToList[ReadPsi4[$NRMMATestSimulation,2,2,100]][[{1,2,200,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadPsi4.m"}]]
  ,
  TestID->"ReadPsi4"
    ]

Test[
  ReadPsi4Radii[$NRMMATestSimulation]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadPsi4Radii.m"}]]
  ,
  TestID->"ReadPsi4Radii"
    ]

Test[
  ExtrapolateScalar[2,Table[{r, 20 + r^-1 + 2 r^-2}, {r, 10, 30, 5}]]
  ,
  19.99999999999999
  ,
  TestID->"ExtrapolateScalar-1"
    ]

Test[
  ExtrapolateScalar[1,Table[{r, 20 + r^-1 + 2 r^-2}, {r, 1, 5, 1}]]
  ,
  19.46294218585005
  ,
  TestID->"ExtrapolateScalar-1"
    ]

Test[
  ToList@ExtrapolateDataTables[1, Table[{r,MakeDataTable[Table[{t, t + 20 + r^-1 + 2 r^-2},
                                                               {t, 0, 10, 5}]]}, 
                                        {r, 1, 5, 1}]]
  ,
  {{0, 19.46294218585005}, {5, 24.46294218585005}, {10, 29.462942185850054}}
  ,
  TestID->"ExtrapolateDataTables-1"
    ]

Test[
  ToList@ExtrapolateDataTables[1, Table[{r,MakeDataTable[Table[{t, t + 20 + r^-1 + 2 r^-2},
                                                               {t, 0, 10, 5}]]}, 
                                        {r, 1, 5, 1}],{2,4}]
  ,
  {{0, 19.73214285714284}, {5, 24.732142857142843}, {10, 29.73214285714284}}
  ,
  TestID->"ExtrapolateDataTables-2"
    ]

Test[
  Map[ToList, 
      AlignPhases[
        Table[MakeDataTable[
          Table[{t, dph + (t - 5)^2}, {t, 0, 10, 1}]], {dph, {0, 2 Pi, 0.1,
                                                              2 Pi + 0.1, 2 Pi - 0.1}}], 5]][[All, 6]],
  {{5, 0.}, {5, 0.}, {5, 0.1}, {5, 0.09999999999999964}, {5, -0.09999999999999964}}
  ,
  TestID->"AlignPhases"
    ]

Test[
  TortoiseCoordinate[r, Madm],
  r + 2 Madm Log[r/(2. Madm) - 1.],
  TestID->"TortoiseCoordinate"
    ]

Test[
ToList[ExtrapolateRadiatedQuantity[
  Table[{r, 
    MakeDataTable[
     Table[{t, t - r + 20 + r^-1 + 2 r^-2}, {t, 0, 100, 5}]]}, {r, 2, 
    50, 1}], UseTortoiseCoordinate -> False, ExtrapolationOrder -> 2]],
  {{-2, 18.000000000000007}, {3, 23.00000000000001}, {8, 28.00000000000001}, {13, 33.00000000000002}, 
   {18, 38.000000000000014}, {23, 43.00000000000001}, {28, 48.000000000000014}, {33, 53.000000000000014}, 
   {38, 58.00000000000004}, {43, 63.000000000000036}, {48, 68.00000000000001}},
  TestID->"ExtrapolateRadiatedQuantity"
    ]

(****************************************************************)
(* StrainFromPsi4 *)
(****************************************************************)

Test[
  Psi4ToStrain[ToDataTable[{{0.0,1+3I},{0.1,5+0.2I}}],0.1]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"StrainFromPsi4-1.m"}]]
  ,
  TestID->"Psi4ToStrain-1"
    ]

Test[
  ToList[Psi4ToStrain[ReadPsi4[$NRMMATestSimulation,2,2,100],0.02]][[{1,2,200,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"StrainFromPsi4-2.m"}]]
  ,
  TestID->"Psi4ToStrain-2"
    ]
