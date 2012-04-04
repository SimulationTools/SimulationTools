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
