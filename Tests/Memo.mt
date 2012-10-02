(* Mathematica Test File *)
Block[{$Memoisation=True},
Test[
	DefineMemoFunction[f1[x_],x]; f1[1],
	1,
	TestID->"memo_1"];

Module[{ran=0},
	Test[
		DefineMemoFunction[f2[x_],ran++;x]; {f2[1],f2[1]}; ran,
		1,
		TestID->"memo_2"]];
	
Module[{ran=0},
	Test[
		DefineMemoFunction[f3[x_],ran++;x];	{f3[1],f3[2],f3[2]}; ran,
		2,
		TestID->"memo_3"]];
	
Module[{ran=0},
	Test[
		DefineMemoFunction[f4[x_],ran++;x];	{f4[1]}; ClearAllMemos[];{f4[1]}; ran,
		2,
		TestID->"memo_4"]];

Module[{ran=0},
	Test[
		DefineMemoFunction[f5[x_],ran++;x];	{f5[1]};DefineMemoFunction[f5[x_],ran++;x];{f5[1]};ran,
		1,
		TestID->"memo_5"]];

Module[{ranInt=0,ranString=0},
	Test[
		DefineMemoFunction[f6[x_Integer],ranInt++;x];DefineMemoFunction[f6[x_String],ranString++;x];
		{f6[3],f6["Hello"]};
		{ranInt,ranString},
		{1,1},
		TestID->"memo_6"]];
		
Module[{ranInt=0,ranString=0},
	Test[
		DefineMemoFunction[f7[x_Integer],ranInt++;x];DefineMemoFunction[f7[x_String],ranString++;x];
		Check[
			ClearAllMemos[];
			ClearAllMemos[];
			True,
			$Failed],
		True,
		TestID->"memo_7"]]
]