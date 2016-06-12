Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

(****************************************************************)
(* MakeDataTable *)
(****************************************************************)

VerificationTest[
	MakeDataTable[{{1,10},{2,11},{3,12}}]
	,
	DataTable[{{1, 2, 3}, {10, 11, 12}}]
	,
	TestID->"MakeDataTable"
]

(****************************************************************)
(* ToDataTable *)
(****************************************************************)

VerificationTest[
	ToDataTable[{{1,10},{2,11},{3,12}}]
	,
	DataTable[{{1, 2, 3}, {10, 11, 12}}]
	,
	TestID->"ToDataTable"
]

(****************************************************************)
(* ToList *)
(****************************************************************)

VerificationTest[
	ToList[DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	{{1,10},{2,11},{3,12}}
	,
	TestID->"ToList"
]

(****************************************************************)
(* IndVar *)
(****************************************************************)

VerificationTest[
	IndVar[DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	{1,2,3}
	,
	TestID->"IndVar"
]

(****************************************************************)
(* DepVar *)
(****************************************************************)

VerificationTest[
	DepVar[DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	{10,11,12}
	,
	TestID->"DepVar"
]

(****************************************************************)
(* ToListOfData *)
(****************************************************************)

VerificationTest[
	ToListOfData[DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	{10,11,12}
	,
	TestID->"ToListOfData"
]

(****************************************************************)
(* ToListOfCoordinates *)
(****************************************************************)

VerificationTest[
	ToListOfCoordinates[DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	{1,2,3}
	,
	TestID->"ToListOfCoordinates"
]

(****************************************************************)
(* ResampleDataTable *)
(****************************************************************)

Module[{d1 = MakeDataTable[Table[{t^3, Sin[t]}, {t, 0, 1, 0.1}]],
        d2 = MakeDataTable[Table[{t, Cos[t]}, {t, 0, 1, 0.1}]]},
  VerificationTest[
	IndVar[ResampleDataTable[d2, d1]]
	,
        IndVar[d1]
	,
	TestID->"ResampleDataTables-nonuniform"
]]

(****************************************************************)
(* UniformGridQ *)
(****************************************************************)

VerificationTest[
  UniformGridQ[MakeDataTable[Table[{t, Cos[t]}, {t, -0.02, 0.9, 0.1}]]]
  ,
  True
  ,
  TestID->"ResampleDataTables-UniformGridQ1"
]

VerificationTest[
  UniformGridQ[MakeDataTable[Table[{t^3, Sin[t]}, {t, 0, 1, 0.1}]]]
  ,
  False
  ,
  TestID->"ResampleDataTables-UniformGridQ2"
]

(****************************************************************)
(* MakeUniform *)
(****************************************************************)

VerificationTest[
  UniformGridQ[MakeUniform[MakeDataTable[Table[{t^3, Sin[t]}, {t, 0, 1, 0.1}]]]]
  ,
  True
  ,
  TestID->"ResampleDataTables-MakeUniform"
]

(****************************************************************)
(* MapData *)
(****************************************************************)

VerificationTest[
	MapData[f,DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	DataTable[{{1,2,3}, {f[10],f[11],f[12]}}]
	,
	TestID->"MapData"
]

(****************************************************************)
(* Map *)
(****************************************************************)

VerificationTest[
	Map[f,DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	DataTable[{{1,2,3},{f[10],f[11],f[12]}}]
	,
	TestID->"Map"
]

(****************************************************************)
(* MapIndVar *)
(****************************************************************)

VerificationTest[
	MapIndVar[f,DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	DataTable[{{1,2,3},{f[1],f[2],f[3]}}]
	,
	TestID->"MapIndVar"
]

(****************************************************************)
(* ApplyToList *)
(****************************************************************)

VerificationTest[
	ApplyToList[Part[#,{1,3}]&,DataTable[{{1, 2, 3}, {10, 11, 12}}]]
	,
	DataTable[{{1, 3}, {10, 12}}]
	,
	TestID->"ApplyToList"
]

(****************************************************************)
(* MapThreadData *)
(****************************************************************)

VerificationTest[
	MapThreadData[f, {
          DataTable[{{1,2,3},{10,11,12}}],
          DataTable[{{1,2,3},{20,21,22}}],
          DataTable[{{1,2,3},{30,31,32}}]}]
	,
	DataTable[{{1,2,3},{f[10,20,30],f[11,21,31],f[12,22,32]}}]
	,
	TestID->"MapThreadData"
]

(****************************************************************)
(* MapThread *)
(****************************************************************)

VerificationTest[
	MapThread[f, {
          DataTable[{{1,2,3},{10,11,12}}],
          DataTable[{{1,2,3},{20,21,22}}],
          DataTable[{{1,2,3},{30,31,32}}]}]
	,
	DataTable[{{1,2,3},{f[10,20,30],f[11,21,31],f[12,22,32]}}]
	,
	TestID->"MapThread"
]

(****************************************************************)
(* MapList *)
(****************************************************************)

VerificationTest[
	MapList[(#[[1]]^2+#[[2]]^2) &,DataTable[{{1,2,3},{10,11,12}}]]
	,
	DataTable[{{1,2,3},{101,125,153}}]
	,
	TestID->"MapList"
]

(****************************************************************)
(* Downsample *)
(****************************************************************)

VerificationTest[
	Downsample[DataTable[{{1,2,3,4},{10,11,12,13}}],2]
	,
	DataTable[{{1,3},{10,12}}]
	,
	TestID->"Downsample"
]

(****************************************************************)
(* Downsampled *)
(****************************************************************)

VerificationTest[
	Downsampled[DataTable[{{1,2,3,4},{10,11,12,13}}],2]
	,
	DataTable[{{1,3},{10,12}}]
	,
	TestID->"Downsampled"
]

(****************************************************************)
(* DataTableInterval *)
(****************************************************************)

VerificationTest[
	DataTableInterval[DataTable[{{1,2,3,4},{10,11,12,13}}],{2,3}]
	,
	DataTable[{{2,3},{11,12}}]
	,
	TestID->"DataTableInterval"
]

(****************************************************************)
(* IntersectDataTables *)
(****************************************************************)

VerificationTest[
        IntersectDataTables[{DataTable[{{1, 2, 3}, {10, 11, 12}}], 
                             DataTable[{{2, 3, 4}, {2, 100, 13}}]}]
	,
	{DataTable[{{2,3},{11,12}}],DataTable[{{2, 3}, {2, 100}}]}
	,
	TestID->"IntersectDataTables"
]

(****************************************************************)
(* LocateMaximumPoint *)
(****************************************************************)

VerificationTest[
        LocateMaximumPoint[DataTable[{{1, 2, 3}, {10, 11, 3}}]]
	,
        2
	,
	TestID->"LocateMaximumPoint"
]

(****************************************************************)
(* LocateMaximum *)
(****************************************************************)

VerificationTest[
        LocateMaximum[MakeDataTable[N@Table[{t, t^2 - t^3}, {t, 0, 1, 0.1}]]]
        ,        
        0.6666666689068567`
	,
	TestID->"LocateMaximum",
	SameTest -> ((Abs[#1-#2] < 3*10^-6) &)
]

(****************************************************************)
(* MaximumValue *)
(****************************************************************)

VerificationTest[
        MaximumValue[MakeDataTable[N@Table[{t, t^2 - t^3}, {t, 0, 1, 0.1}]]]
        ,        
        0.14814814814814817`
	,
	TestID->"MaximumValue",
	SameTest -> ((Abs[#1-#2] < 9*10^-12) &)
]

(****************************************************************)
(* InvertDataTable *)
(****************************************************************)

VerificationTest[
        InvertDataTable[DataTable[{{1,2,3,4},{10,11,12,15}}]]
        ,        
        DataTable[{{10,11,12,15},{1,2,3,4}}]
	,
	TestID->"InvertDataTable"
]

(****************************************************************)
(* MonotonicQ *)
(****************************************************************)

VerificationTest[
        MonotonicQ[DataTable[{{1,2,3,4},{10,11,12,15}}]]
        ,        
        True
	,
	TestID->"MonotonicQ-1"
]

VerificationTest[
        MonotonicQ[DataTable[{{1,3,2,4},{10,11,12,15}}]]
        ,        
        False
	,
	TestID->"MonotonicQ-2"
]

(****************************************************************)
(* PartitionTable *)
(****************************************************************)

VerificationTest[

        SimulationTools`DataTable`Private`partitionTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}, {5, 2}, {6,3}},
                       {2.5, 4.5}]
        ,
        {{{1, 10}, {2, 11}}, {{3, 12}, {4, 15}}, {{5, 2}, {6, 3}}}
	,
	TestID->"partitionTable-1"
]

VerificationTest[

        SimulationTools`DataTable`Private`partitionTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}, {5, 2}, {6,3}},
                       {2, 4}]
        ,
        {{{1, 10}}, {{2, 11}, {3, 12}}, {{4, 15}, {5, 2}, {6, 3}}}
	,
	TestID->"partitionTable-2"
]

(****************************************************************)
(* ShiftDataTable *)
(****************************************************************)

VerificationTest[
        ShiftDataTable[2, DataTable[{{1,2,3,4},{10,11,12,15}}]]
        ,
        DataTable[{{3,4,5,6},{10,11,12,15}}]
	,
	TestID->"ShiftDataTable"
]

(****************************************************************)
(* Spacing *)
(****************************************************************)

VerificationTest[
        Spacing[DataTable[{{1,2,3,4},{10,11,12,15}}]]
        ,
        1
	,
	TestID->"Spacing"
]

(****************************************************************)
(* DataTableRange *)
(****************************************************************)

VerificationTest[
        DataTableRange[DataTable[{{1,2,3,4},{10,11,12,15}}]]
        ,
        {1,4}
	,
	TestID->"DataTableRange"
]



(* Have tests *)

ApplyToList
DataTableInterval
DataTableRange
DepVar
Downsample
Downsampled
IndVar
IntersectDataTables
InvertDataTable
LocateMaximum
LocateMaximumPoint
MakeDataTable
MakeUniform
Map
MapData
MapIndVar
MapList
MapThread
MapThreadData
MaximumValue
MonotonicQ
PartitionTable
Phase
ResampleDataTable
ShiftDataTable
Spacing
ToDataTable
ToList
ToListOfCoordinates
ToListOfData
UniformGridQ

(* Need tests *)

Add
DataTableDepVarInterval
DataTableListLinePlot
DataTableNormL2
Div
FilterDCT
Frequency
IntegrateDataTable
IntegrateDataTableZeroEnd
IntegrateDataTableZeroStart
InterpolateWhereFunction
MakeInterpolatingDataTable
NDerivative
PhaseOfFrequency
ResampleDataTables
ShiftPhase
Sub

(* Won't test *)

AbsOfPhase
AddAttribute
AddAttributes
FunctionOfPhase
ListAttributes
ReadAttribute
TableRange
