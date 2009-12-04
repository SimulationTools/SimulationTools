
BeginPackage["Movie`"];

PadIndex;
PreviewMovie;
MakeMovie;

Begin["`Private`"];

PadIndex[i_, n_] :=
 Module[{str, prefix, l},
  str = ToString[i];
  l = StringLength[str];
  If[l >= n, str,
   prefix = StringJoin@ConstantArray["0", n - l];
   prefix <> str]]

SetAttributes[PreviewMovie, HoldAll];

PreviewMovie[fileNamebase_String, expr_, {var_, v1_, v2_, dv_:1}] :=
  Manipulate[expr, {var, v1, v2, dv}];

SetAttributes[MakeMovie, HoldAll];

MakeMovie[fileNameBase_String, expr_, {var_, v1_, v2_, dv_:1}] :=
  Module[{name},
    i1 = 0;
    i2 = (v2 - v1) / dv;
    Do[
      name = Evaluate[fileNameBase] <> "." <> PadIndex[i,5] <> ".png";
      Export[name, Block[{var=v1 + i dv}, expr]];
      , {i, i1, i2}];

    command = "! /Users/ian/Software/bin/ffmpeg -y -i " <> Evaluate[fileNameBase] <> ".%5d.png -b 100000 " <> fileNameBase <> ".mp4 2>&1";
    Print[command];
    ReadList[command, String, NullRecords->True]
  ];


End[];

EndPackage[];
