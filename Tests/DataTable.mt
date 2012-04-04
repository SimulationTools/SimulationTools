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
