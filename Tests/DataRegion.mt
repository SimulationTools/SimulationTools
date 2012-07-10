(* Mathematica Test File *)

data  = Table[100 i + 10 j + k, {i, 2}, {j, 3}, {k, 4}];
data2 = 2 data;

dr = ToDataRegion[data, {10, 20, 30}, {0.1, 0.2, 0.3},
   VariableName -> "test region", Time -> 42];
dr2 = ToDataRegion[data2, {10, 20, 30}, {0.1, 0.2, 0.3},
   VariableName -> "test region", Time -> 47];
dr3 = ToDataRegion[data2, {9.9, 20, 30}, {0.1, 0.2, 0.3},
   VariableName -> "test region", Time -> 47];

(****************************************************************)
(* ToDataRegion *)
(****************************************************************)
Test[
    ToDataRegion[data, {10, 20, 30}, {0.1, 0.2, 0.3},
    	VariableName -> "test region", Time -> 42]
    ,
    DataRegion[{VariableName -> "test region",
        DataRegion`Private`Origin -> {10., 20., 30.}, Spacing -> {0.1, 0.2, 0.3},
        DataRegion`Private`Time -> 42},
        data]
    ,
    TestID->"ToDataRegion"
]


(****************************************************************)
(* ToListOfData *)
(****************************************************************)
Test[
    ToListOfData[dr, Flatten -> False]
    ,
    data
    ,
    TestID->"ToListOfData"
]





(****************************************************************)
(************************* To be moved **************************)
(****************************************************************)

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


(****************************************************************)
(* TimeDerivative *)
(****************************************************************)
Test[
    TimeDerivative[{dr, dr2}]
    ,
    DataRegion[{VariableName -> "dt_test region",
      DataRegion`Private`Origin -> {10., 20., 30.},
      Spacing -> {0.1, 0.2, 0.3}, 
      DataRegion`Private`Time -> 89/2},
      {{{111/5, 112/5, 113/5, 114/5}, {121/5, 122/5, 123/5, 124/5}, {131/5, 132/5, 133/5, 134/5}}, 
      {{211/5, 212/5, 213/5, 214/5}, {221/5, 222/5, 223/5, 224/5}, {231/5, 232/5, 233/5, 234/5}}}]
    ,
    TestID->"TimeDerivative"
]


