(* Mathematica Test File *)

(****************************************************************)
(* MakeDataTable *)
(****************************************************************)

Test[
	MakeDataTable[{{1,10},{2,11},{3,12}}]
	,
	DataTable[{{1,10},{2,11},{3,12}}]
	,
	TestID->"MakeDataTable"
]

(****************************************************************)
(* ToDataTable *)
(****************************************************************)

Test[
	ToDataTable[{{1,10},{2,11},{3,12}}]
	,
	DataTable[{{1,10},{2,11},{3,12}}]
	,
	TestID->"ToDataTable"
]

(****************************************************************)
(* ToList *)
(****************************************************************)

Test[
	ToList[DataTable[{{1,10},{2,11},{3,12}}]]
	,
	{{1,10},{2,11},{3,12}}
	,
	TestID->"ToList"
]

(****************************************************************)
(* IndVar *)
(****************************************************************)

Test[
	IndVar[DataTable[{{1,10},{2,11},{3,12}}]]
	,
	{1,2,3}
	,
	TestID->"IndVar"
]

(****************************************************************)
(* DepVar *)
(****************************************************************)

Test[
	DepVar[DataTable[{{1,10},{2,11},{3,12}}]]
	,
	{10,11,12}
	,
	TestID->"DepVar"
]

(****************************************************************)
(* ToListOfData *)
(****************************************************************)

Test[
	ToListOfData[DataTable[{{1,10},{2,11},{3,12}}]]
	,
	{10,11,12}
	,
	TestID->"ToListOfData"
]

(****************************************************************)
(* ToListOfCoordinates *)
(****************************************************************)

Test[
	ToListOfCoordinates[DataTable[{{1,10},{2,11},{3,12}}]]
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
  Test[
	IndVar[ResampleDataTable[d2, d1]]
	,
        IndVar[d1]
	,
	TestID->"ResampleDataTables-nonuniform"
]]

(****************************************************************)
(* UniformGridQ *)
(****************************************************************)

Test[
  UniformGridQ[MakeDataTable[Table[{t, Cos[t]}, {t, -0.02, 0.9, 0.1}]]]
  ,
  True
  ,
  TestID->"ResampleDataTables-UniformGridQ1"
]

Test[
  UniformGridQ[MakeDataTable[Table[{t^3, Sin[t]}, {t, 0, 1, 0.1}]]]
  ,
  False
  ,
  TestID->"ResampleDataTables-UniformGridQ2"
]

(****************************************************************)
(* MakeUniform *)
(****************************************************************)

Test[
  UniformGridQ[MakeUniform[MakeDataTable[Table[{t^3, Sin[t]}, {t, 0, 1, 0.1}]]]]
  ,
  True
  ,
  TestID->"ResampleDataTables-MakeUniform"
]

(****************************************************************)
(* MapData *)
(****************************************************************)

Test[
	MapData[f,DataTable[{{1,10},{2,11},{3,12}}]]
	,
	DataTable[{{1,f[10]},{2,f[11]},{3,f[12]}}]
	,
	TestID->"MapData"
]

(****************************************************************)
(* Map *)
(****************************************************************)

Test[
	Map[f,DataTable[{{1,10},{2,11},{3,12}}]]
	,
	DataTable[{{1,f[10]},{2,f[11]},{3,f[12]}}]
	,
	TestID->"Map"
]

(****************************************************************)
(* MapIndVar *)
(****************************************************************)

Test[
	MapIndVar[f,DataTable[{{1,10},{2,11},{3,12}}]]
	,
	DataTable[{{1,f[1]},{2,f[2]},{3,f[3]}}]
	,
	TestID->"MapIndVar"
]

(****************************************************************)
(* ApplyToList *)
(****************************************************************)

Test[
	ApplyToList[f,DataTable[{{1,10},{2,11},{3,12}}]]
	,
	DataTable[f[{{1,10},{2,11},{3,12}}]]
	,
	TestID->"ApplyToList"
]

(****************************************************************)
(* MapThreadData *)
(****************************************************************)

Test[
	MapThreadData[f, {
          DataTable[{{1,10},{2,11},{3,12}}],
          DataTable[{{1,20},{2,21},{3,22}}],
          DataTable[{{1,30},{2,31},{3,32}}]}]
	,
	DataTable[{{1,f[10,20,30]},{2,f[11,21,31]},{3,f[12,22,32]}}]
	,
	TestID->"MapThreadData"
]

(****************************************************************)
(* MapThread *)
(****************************************************************)

Test[
	MapThread[f, {
          DataTable[{{1,10},{2,11},{3,12}}],
          DataTable[{{1,20},{2,21},{3,22}}],
          DataTable[{{1,30},{2,31},{3,32}}]}]
	,
	DataTable[{{1,f[10,20,30]},{2,f[11,21,31]},{3,f[12,22,32]}}]
	,
	TestID->"MapThread"
]

(****************************************************************)
(* MapList *)
(****************************************************************)

Test[
	MapList[(#[[1]]^2+#[[2]]^2) &,DataTable[{{1,10},{2,11},{3,12}}]]
	,
	DataTable[{{1,101},{2,125},{3,153}}]
	,
	TestID->"MapList"
]

(****************************************************************)
(* Downsample *)
(****************************************************************)

Test[
	Downsample[DataTable[{{1,10},{2,11},{3,12},{4,13}}],2]
	,
	DataTable[{{1,10},{3,12}}]
	,
	TestID->"Downsample"
]

(****************************************************************)
(* Downsampled *)
(****************************************************************)

Test[
	Downsampled[DataTable[{{1,10},{2,11},{3,12},{4,13}}],2]
	,
	DataTable[{{1,10},{3,12}}]
	,
	TestID->"Downsampled"
]

(****************************************************************)
(* DataTableInterval *)
(****************************************************************)

Test[
	DataTableInterval[DataTable[{{1,10},{2,11},{3,12},{4,13}}],{2,3}]
	,
	DataTable[{{2,11},{3,12}}]
	,
	TestID->"DataTableInterval"
]

(****************************************************************)
(* IntersectDataTables *)
(****************************************************************)

Test[
        IntersectDataTables[{DataTable[{{1, 10}, {2, 11}, {3, 12}}], 
                             DataTable[{{2, 2}, {3, 100}, {4, 13}}]}]
	,
	{DataTable[{{2,11},{3,12}}],DataTable[{{2, 2}, {3, 100}}]}
	,
	TestID->"IntersectDataTables"
]

(****************************************************************)
(* LocateMaximumPoint *)
(****************************************************************)

Test[
        LocateMaximumPoint[DataTable[{{1, 10}, {2, 11}, {3, 3}}]]
	,
        2
	,
	TestID->"LocateMaximumPoint"
]

(****************************************************************)
(* LocateMaximum *)
(****************************************************************)

Test[
        LocateMaximum[MakeDataTable[N@Table[{t, t^2 - t^3}, {t, 0, 1, 0.1}]]]
        ,        
        0.6666666689068567`
	,
	TestID->"LocateMaximum"
]

(****************************************************************)
(* MaximumValue *)
(****************************************************************)

Test[
        MaximumValue[MakeDataTable[N@Table[{t, t^2 - t^3}, {t, 0, 1, 0.1}]]]
        ,        
        0.14814814814814817`
	,
	TestID->"MaximumValue"
]

(****************************************************************)
(* InvertDataTable *)
(****************************************************************)

Test[
        InvertDataTable[DataTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}}]]
        ,        
        DataTable[{{10, 1}, {11, 2}, {12, 3}, {15, 4}}]
	,
	TestID->"InvertDataTable"
]

(****************************************************************)
(* MonotonicQ *)
(****************************************************************)

Test[
        MonotonicQ[DataTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}}]]
        ,        
        True
	,
	TestID->"MonotonicQ-1"
]

Test[
        MonotonicQ[DataTable[{{1, 10}, {3, 11}, {2, 12}, {4, 15}}]]
        ,        
        False
	,
	TestID->"MonotonicQ-2"
]

(****************************************************************)
(* PartitionTable *)
(****************************************************************)

Test[

        SimulationTools`DataTable`Private`partitionTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}, {5, 2}, {6,3}},
                       {2.5, 4.5}]
        ,
        {{{1, 10}, {2, 11}}, {{3, 12}, {4, 15}}, {{5, 2}, {6, 3}}}
	,
	TestID->"partitionTable-1"
]

Test[

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

Test[
        ShiftDataTable[2, DataTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}}]]
        ,
        DataTable[{{3, 10}, {4, 11}, {5, 12}, {6, 15}}]
	,
	TestID->"ShiftDataTable"
]

(****************************************************************)
(* Spacing *)
(****************************************************************)

Test[
        Spacing[DataTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}}]]
        ,
        1
	,
	TestID->"Spacing"
]

(****************************************************************)
(* DataTableRange *)
(****************************************************************)

Test[
        DataTableRange[DataTable[{{1, 10}, {2, 11}, {3, 12}, {4, 15}}]]
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
