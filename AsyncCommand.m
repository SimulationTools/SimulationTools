(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["AsyncCommand`"];

ExecuteAsyncCommand;
CommandReadyQ;
CommandOutputFile;
CommandErrorFile;
CommandOutput;
CommandError;
CommandReturnCode;
CommandClose;
AsyncCommand;

Begin["`Private`"];

ExecuteAsyncCommand[cmd_String] :=
  Module[{prefix, outFile, errFile, retDoneFile, s, 
   retFile},
  prefix = $HomeDirectory <> "/asc." <> ToString[Round[AbsoluteTime[]*100]];
  outFile = prefix <> ".out";
  errFile = prefix <> ".err";
  retFile = prefix <> ".ret";
  retDoneFile = prefix <> ".ret.done";

  s = OpenRead[
    "!" <> cmd <> "</dev/null >" <> outFile <> " 2>" <> errFile <> 
     " ; echo $? >" <> retFile <> "; mv "<>retFile<>" "<>retDoneFile];
  Return[AsyncCommand[
    cmd, {Stream -> s, OutFile -> outFile, ErrFile -> errFile, 
     RetFile -> retFile, RetDoneFile -> retDoneFile}]];
  ];

CommandReadyQ[AsyncCommand[cmd_, dat_]] :=
  FileExistsQ[RetDoneFile /. dat];

CommandOutputFile[AsyncCommand[cmd_, dat_]] :=
 OutFile /. dat;

CommandOutput[asc:AsyncCommand[cmd_, dat_]] :=
  If[FileExistsQ[CommandOutputFile[asc]],
     ReadList[CommandOutputFile[asc], String],
     {}];

CommandErrorFile[AsyncCommand[cmd_, dat_]] :=
 ErrFile /. dat;

CommandError[asc:AsyncCommand[cmd_, dat_]] :=
 ReadList[CommandErrorFile[asc], String];

CommandReturnCode[asc:AsyncCommand[cmd_, dat_]] :=
  If[CommandReadyQ[asc],
    ReadList[RetDoneFile /. dat, Number][[1]],
    Error["Command is not ready"]];

CommandClose[AsyncCommand[cmd_, dat_]] :=
 Module[{},
  Quiet[Scan[
    DeleteFile, {OutFile, ErrFile, 
      RetFile, RetDoneFile} /. dat], DeleteFile::nffil];
  Close[Stream /. dat];
  ];

End[];

EndPackage[];
