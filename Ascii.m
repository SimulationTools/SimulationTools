(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Ascii`",
 {
  "Ascii1D`",
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "Profile`",
  "RunFiles`"
 }];

ReadCarpetASCIIScalar::usage = "ReadCarpetASCIIScalar[run,filename] Reads Carpet ASCII 0D and Scalar data.";

Begin["`Private`"];

ReadCarpetASCIIScalar[run_,fileName_] :=
 Module[{coltime, coldata, data},
(*  lines = ReadList[fileName, String, NullRecords -> True];*)
  (*If[FileType[fileName] === None, Error["ReadCarpetASCIIScalar: File "<>fileName<>" not found"]];*)
  Profile["ReadCarpetASCIIScalar: Check if we want to plot scalar output or 0D output. They have different formats.",
    If[StringLength[StringSplit[FileNameTake[fileName],"."][[2]]]<= 2,coltime=9;coldata=13;,coltime=2;coldata=3;]];
  Profile["ReadCarpetASCIIScalar: " <> fileName,
  Profile["ReadCarpetASCIIScalar: Reading file", 
    data = MakeDataTable[ReadColumnFile[run,fileName,{coltime,coldata}]]]]];
End[];

EndPackage[];
