(* Mathematica Test File *)

data = {{{111, 112, 113, 114}, {121, 122, 123, 124}, {131, 132, 133, 134}},
	    {{211, 212, 213, 214}, {221, 222, 223, 224}, {231, 232, 233, 234}}};

(****************************************************************)
(* ToDataRegion *)
(****************************************************************)
Test[
    ToDataRegion[data, {10, 20, 30}, {0.1, 0.2, 0.3},
    	Name -> "test region", Time -> 42]
    ,
    DataRegion[{DataRegion`Private`Name -> "test region",
        DataRegion`Private`Origin -> {10, 20, 30}, Spacing -> {0.1, 0.2, 0.3},
        DataRegion`Private`Time -> 42},
        data]
    ,
    TestID->"MakeDataRegion"
]

(****************************************************************)
(************************** DEPRECATED **************************)
(****************************************************************)

(****************************************************************)
(* MakeDataRegion *)
(****************************************************************)
Test[
    MakeDataRegion[data, "test region", {4, 3, 2}, {10, 20, 30}, {0.1, 0.2, 0.3}, 42]
    ,
    DataRegion[{DataRegion`Private`Name -> "test region",
    	DataRegion`Private`Origin -> {10, 20, 30}, Spacing -> {0.1, 0.2, 0.3}, 
        DataRegion`Private`Time -> 42},
        data]
    ,
    TestID->"MakeDataRegion"
]