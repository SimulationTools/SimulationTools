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
