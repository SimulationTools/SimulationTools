(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Movie`", {"Error`"}];

PadIndex;
PreviewMovie;
MakeMovie::usage = "MakeMovie[filenamebase, expression, {var, v1, v2, dv}] evaluates expression (which should be a graphical object) for values of var from v1 to v2 in steps of dv and writes the result to a PNG file filenamebase with an index number appended.  The images are then combined into an MP4 file using FFMPEG.";

Begin["`Private`"];

PadIndex[i_, n_] :=
 Module[{str, prefix, l},
  str = ToString[i];
  l = StringLength[str];
  If[l >= n, str,
   prefix = StringJoin@ConstantArray["0", n - l];
   prefix <> str]]

SetAttributes[PreviewMovie, HoldAll];

PreviewMovie[fileNamebase_, expr_, {var_, v1_, v2_, dv_:1}] :=
  Manipulate[expr, {var, v1, v2, dv}];

SetAttributes[MakeMovie, HoldAll];

Options[MakeMovie] = {"FFMPEG" -> "ffmpeg",
                      "FrameRate" -> 1};

MakeMovie[fileNameBase_, expr_, {var_, v1_, v2_, dv_:1}, opts:OptionsPattern[]] :=
  Module[{name, i1, i2, command},
    i1 = 0;
    i2 = (v2 - v1) / dv;
    Do[
      name = Evaluate[fileNameBase] <> "." <> PadIndex[i,5] <> ".png";
      Export[name, Block[{var=v1 + i dv}, expr]];
      , {i, i1, i2}];

    command = "! "<>OptionValue["FFMPEG"]<>" -y -r "<>ToString[OptionValue["FrameRate"]//N,CForm]<>" -i " <> Evaluate[fileNameBase] <> ".%5d.png -b 100000 " <> Evaluate[fileNameBase] <> ".mp4 2>&1";
    Print[command];
    ReadList[command, String, NullRecords->True]
  ];

End[];

EndPackage[];
