(* Mathematica Test File *)

(****************************************************************)
(* FilterNaNs *)
(****************************************************************)
Test[
    FilterNaNs[ToDataRegion[{-2147483648}, {10}, {0.1}, "VariableName" -> "test region"]]
    ,
    DataRegion[{"VariableName" -> "test region", "Origin" -> {10.}, "Spacing" -> {0.1}}, {Missing[]}]
    ,
    TestID->"FilterNaNs"
]


(****************************************************************)
(* NaNQ *)
(****************************************************************)
Test[
    NaNQ[-2147483648]
    ,
    True
    ,
    TestID->"NaNQ"
]


