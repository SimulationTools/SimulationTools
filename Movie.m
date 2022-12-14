(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["SimulationTools`Movie`", {"SimulationTools`Error`",
                                        "SimulationTools`Utils`"}];

PadIndex;
PreviewMovie;
MakeMovie(*::usage = "MakeMovie[filenamebase, expression, {var, v1, v2, dv}] evaluates expression (which should be a graphical object) for values of var from v1 to v2 in steps of dv and writes the result to a PNG file filenamebase with an index number appended.  The images are then combined into an MP4 file using FFMPEG."*);

ExportMovie;
EncodeMovieFrameFiles;
ExportMovieFrames;

CreateFrameSet;
RenderFrames;
ManipulateFrames;
SaveFrameSet;
LoadFrameSet;
ClearFrameData;
ClearFrame;
ListFrames;
ReadFrameData;
RenderFrame;
ExportFrameSetMovie;
$ExportMovieProgress;
$ExportMovieFramesStatus;

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

(* New *)

ExportMovieFrames[fileBase_String, frames_List, opts:OptionsPattern[]] :=
  Do[
    (* TODO: handle the case where there are files here already which
       match the pattern. Maybe put them in a new temporary
       directory. *)
    $ExportMovieProgress = N[i/Length[frames]];
    $ExportMovieFramesStatus = N[i/Length[frames]];
    Export[fileBase<>"."<>PadIndex[i-1,5]<>".png",
           frames[[i]]],
    {i,1,Length[frames]}];

findFfmpeg[] := 
  Module[
    {ret,out,err},
    (* TODO: running a user's profile might be slow; cache this? *)
    {ret,out,err} = RunSubprocess[{"bash", "-l", "-c", "\"which ffmpeg\""}];
    If[ret =!= 0, Error["Cannot find ffmpeg"]];
    Last[out]];

Options[EncodeMovieFrameFiles] = {"FFMPEG" -> "ffmpeg",
                                  "FrameRate" -> 1};

EncodeMovieFrameFiles[movieFile_String, frameFilePattern_String, OptionsPattern[]] :=
  Module[
    {cmd},

    cmd = {findFfmpeg[],
           "-y", 
           "-f", "image2",
           "-r", ToString[N@OptionValue["FrameRate"],CForm],
           "-i", frameFilePattern,
           "-b:v", "20M",
           "-pix_fmt", "yuv420p",
           movieFile};
    Print[cmd];
    {ret, out, err} = RunSubprocess[cmd];
    Print[out];
    Print[err];
    If[ret =!= 0, Error["Failed to encode movie frame files to "<>movieFile<>". \n"<>err]];
    ];

Options[ExportMovie] = {"FFMPEG" -> "ffmpeg",
  "FrameRate" -> 1, "Parallel" -> False};

ExportMovie[movieFile_String, frames_List, opts:OptionsPattern[]] :=
  Module[
    {framesDir = FileNameJoin[{$TemporaryDirectory,FileBaseName[movieFile]}],
     base},

    If[!DirectoryQ[framesDir], CreateDirectory[framesDir]];
    base = FileNameJoin[{framesDir,"frame"}];
    (* TODO: what about extra files from previous longer movies? *)
    ExportMovieFrames[base, frames, opts];
    EncodeMovieFrameFiles[movieFile, base <> ".%5d.png", opts]];

ExportMovie[movieFile_String, frameFn_, args_List, opts:OptionsPattern[]] :=
  Module[
    {framesDir, base, j, k, status, timeout=180, inds, exportFrame, failures, lastFailures,
    parallel = OptionValue[Parallel]},

    inds = Range[1,Length[args]];

    framesDir = StringReplace[movieFile, Shortest["."~~ext__~~EndOfString] :> ".frames"];

    If[!DirectoryQ[framesDir], CreateDirectory[framesDir]];
    base = FileNameJoin[{framesDir,"frame"}];

    generateFrame[ind_Integer] :=
    Module[{pngfile, temp, mFile},
      pngfile = base<>"."<>PadIndex[ind-1,5]<>".png";
      (* mFile = StringReplace[pngfile,".png"->".m"]; *)
      mFile = pngfile<>".mx";
      temp = mFile <> ".tmp";
      If[FileExistsQ[temp],
        DeleteFile[temp]];
      If[FileExistsQ[mFile],
        Print["Skipping existing ", mFile];
        None,
        Print["Generating frame ", ind-1, " for argument ", args[[ind]]];
        {time,img} = AbsoluteTiming[frameFn[args[[ind]]]];
        Print["Frame generation took ", time, " s"];
        Export[temp, img, "MX"];
        RenameFile[temp, mFile];
        Print[mFile]]];

    exportFrame[ind_Integer] :=
    Module[{pngfile, temp, mFile, img},
      pngfile = base<>"."<>PadIndex[ind-1,5]<>".png";
      temp = pngfile <> ".tmp";
      (* mFile = StringReplace[pngfile,".png"->".m"]; *)
      mFile = pngfile<>".mx";
      If[FileExistsQ[pngfile],
        True,
        (* else *)
        Print["Exporting frame ", ind-1, " for argument ", args[[ind]]];
        img = Import[mFile, "MX"];
        Print["Exporting"];
        Export[temp, img, "PNG"];
        Print["Export done"];
        RenameFile[temp, pngfile];
        Print[pngfile];
        True]];

    failures = Infinity;
    lastFailures = Infinity;
    While[failures > 0,
    status = If[parallel,
      DistributeDefinitions[timeout,exportFrame,ind,inds];
      ParallelTable[TimeConstrained[CheckAbort[generateFrame[ind], $Aborted],timeout,Print["Frame ", ind-1, " took more than ", timeout, " seconds to generate; skipping."]; $Timeout], {ind, inds}, Method -> "FinestGrained" ]
      ParallelTable[exportFrame[ind], {ind, inds}, Method -> "FinestGrained"],

      (* else *)

      (* Print["exportMovie: inds = ", inds]; *)
      (* Do export only on the master kernel *)
      Table[generateFrame[ind]; exportFrame[ind], {ind, inds}]];

      failures = Count[status, $Timeout|$Aborted];

      If[failures === lastFailures,
        Print["There were ", failures, " failures, which is the same as on the previous run. Skipping remaining frames."];
        failures = 0];

      If[failures > 0,
        Print["There were ", failures, " failures. Retrying."];
        If[parallel,ParallelEvaluate[Get["h5mma`"]],Get["h5mma`"]];
        Pause[5]]];

    EncodeMovieFrameFiles[movieFile, base <> ".%5d.png",
      FilterRules[{opts},Options[EncodeMovieFrameFiles]]]];

(* TODO: given a base name, create the movie in a temporary location,
   and return the full path to it, or reveal it in the finder, so it
   can be dragged into an email, or uploaded somewhere. *)


(* Frame sets *)

CreateFrameSet[sym_Symbol, listFramesFn_, readFrameDataFn_, 
  renderFrameFn_] :=
 Module[{},
  sym /: ListFrames[sym] := listFramesFn[];
  sym /: ReadFrameData[sym, frameID_] := 
   sym /: ReadFrameData[sym, frameID] = 
    With[{r = readFrameDataFn[frameID]}, (sym /: 
       haveFrameData[sym, frameID] = True); r];
  sym /: RenderFrame[sym, frameID_] := 
   sym /: RenderFrame[sym, frameID] = 
    With[{r = renderFrameFn[ReadFrameData[sym, frameID]]}, (sym /: 
       haveFrame[sym, frameID] = True); r];
  sym /: frameFunctions[sym] = {listFramesFn, readFrameDataFn, 
    renderFrameFn};
  sym /: haveFrameData[sym, _] = False;
  sym /: haveFrame[sym, _] = False;
  ]

RenderFrames[sym_, range_: All] :=
 MapMonitored[RenderFrame[sym, #] &, ListFrames[sym][[range]]]

frameSequence[is_List] :=
 If[And @@ Map[IntegerQ, is] && 
   Length[Union[Drop[is - RotateLeft[is], -1]]] === 1,
  {is[[1]], is[[-1]], is[[2]] - is[[1]]},
  {is}]

ManipulateFrames[sym_, range_: All] :=
 Module[{},
  RenderFrames[sym, range];
  Manipulate[
   RenderFrame[sym, i], {i, 
    Sequence @@ frameSequence[ListFrames[sym][[range]]], 
    ControlType -> Manipulator}]]

SaveFrameSet[sym_Symbol, dir_String] :=
 Module[{},
  If[FileType[dir] =!= Directory, CreateDirectory[dir]];
  Do[If[haveFrameData[sym, i],
    Put[ReadFrameData[sym, i], 
     FileNameJoin[{dir, "data." <> ToString[i] <> ".m"}]]], {i, 
    ListFrames[sym]}];
  Do[If[haveFrame[sym, i],
    Put[RenderFrame[sym, i], 
     FileNameJoin[{dir, "frame." <> ToString[i] <> ".m"}]]], {i, 
    ListFrames[sym]}];
  ]

LoadFrameSet[sym_Symbol, dir_String] :=
 Module[{},
  If[FileType[dir] =!= Directory, 
   Error["Frame set directory not found"]];
  Do[With[{f = FileNameJoin[{dir, "data." <> ToString[i] <> ".m"}]},
    If[FileExistsQ[f], sym /: ReadFrameData[sym, i] = Get[f]; 
     sym /: haveFrameData[sym, i] = True]], {i, ListFrames[sym]}];
  
  (*Do[With[{f=FileNameJoin[{dir,"frame."<>ToString[i]<>".m"}]},
  If[FileExistsQ[f],sym/:RenderFrame[sym,i]=Get[f];sym/:haveFrame[sym,
  i]=True]],{i,ListFrames[sym]}];*)
  ]

ClearFrameData[sym_Symbol, frameID_] :=
 Module[{},
  Print["Clearing data for ", sym, " frame ", frameID];
  sym /: ReadFrameData[sym, frameID] =.;
  sym /: haveFrameData[sym, frameID] = False]

ClearFrame[sym_Symbol, frameID_] :=
 Module[{},
  Print["Clearing frame for ", sym, " frame ", frameID];
  sym /: RenderFrame[sym, frameID] =.;
  sym /: haveFrame[sym, frameID] = False];

Options[ExportFrameSetMovie] = Options[ExportMovie]
ExportFrameSetMovie[sym_Symbol, movieFile_String, range_: All, 
  opts : OptionsPattern[]] :=
 ExportMovie[movieFile, RenderFrames[sym, range], opts]

End[];

EndPackage[];
