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

BeginPackage["SimulationTools`AsyncCommand`",
 {
  "SimulationTools`Error`"
 }];

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
