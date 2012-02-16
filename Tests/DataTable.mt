(* Mathematica Test File *)

(******************************* DataTable *********************************)

(* Converting to and from DataTables *)
Test[
	MakeDataTable[{{1,10},{2,11},{3,12}}]
	,
	DataTable[{{1,10},{2,11},{3,12}}]
	,
	TestID->"MakeDataTable"
]

Test[
	ToList[DataTable[{{1,10},{2,11},{3,12}}]]
	,
	{{1,10},{2,11},{3,12}}
	,
	TestID->"ToList"
]

Module[{d1 = MakeDataTable[Table[{t^3, Sin[t]}, {t, 0, 1, 0.1}]],
        d2 = MakeDataTable[Table[{t, Cos[t]}, {t, 0, 1, 0.1}]]},
  Test[
	IndVar[ResampleDataTable[d2, d1]]
	,
        IndVar[d1]
	,
	TestID->"ResampleDataTables-nonuniform"
]]

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

Test[
  UniformGridQ[MakeUniform[MakeDataTable[Table[{t^3, Sin[t]}, {t, 0, 1, 0.1}]]]]
  ,
  True
  ,
  TestID->"ResampleDataTables-MakeUniform"
]
