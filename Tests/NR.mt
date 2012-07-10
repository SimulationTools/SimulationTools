(* Mathematica Test File *)

(****************************************************************)
(* FilterNaNs *)
(****************************************************************)
Test[
    FilterNaNs[ToDataRegion[{-2147483648}, {10}, {0.1}, VariableName -> "test region", Time -> 42]]
    ,
    DataRegion[{VariableName -> "test region", DataRegion`Private`Origin -> {10.}, Spacing -> {0.1}, DataRegion`Private`Time -> 42}, {Missing[]}]
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


