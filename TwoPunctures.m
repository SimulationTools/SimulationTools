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

BeginPackage["TwoPunctures`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Memo`",
  "Parameters`",
  "RunFiles`"
 }];

ReadPunctureADMMasses(*::usage = "ReadPunctureADMMasses[run] reads the ADM masses of the punctures in run as computed by the TwoPunctures thorn."*);
ReadPunctureADMMassParameters(*::usage  = "ReadPunctureADMMassParameters[run] reads the ADM masses of the punctures in run as requested by the target_M_plus and target_M_minus parameters of the TwoPunctures thorn."*);
TotalMass;
InitialSpinAngularMomentum;
InitialLinearMomentum;
InitialOrbitalAngularMomentum;
InitialAngularMomentum;

MassRatio;
SymmetricMassRatio;
ReadTwoPuncturesData(*::usage = "ReadTwoPuncturesData[file, col] reads a data file output by the standalone TwoPunctures code by Marcus Ansorg and returns a DataRegion containing the data in column col. col can be 1, 2 or 3 for the coordinates or >= 4 for the data."*);

Begin["`Private`"];

TwoPunctures`InitialData`HaveData[run_String, ___] :=
  HaveRunDir[run] && (
    FindRunFile[run, "ADM_mass_tot.asc"] =!= {} ||
    StandardOutputOfRun[run] =!= {});

TwoPunctures`InitialData`ReadADMMass[runName_String] :=
  Module[{massMDFiles, output, lines},
    massMDFiles = FindRunFile[runName, "ADM_mass_tot.asc"];
    If[massMDFiles =!= {},
      Return[ReadList[massMDFiles[[1]], Real][[1]]],
      (* Else *)
      output = StandardOutputOfRun[runName];
      If[Length[output] < 1,
        Error["Cannot find standard output for run "<>runName]];

      lines = Select[ReadList[output[[1]], String], StringMatchQ[#, __ ~~ "total ADM mass is" ~~ __] &, 1];
      If[Length[lines] < 1,
        lines = Select[ReadList[output[[1]], String], StringMatchQ[#, __ ~~ "ADM mass is" ~~ __] &, 1];
        If[Length[lines] < 1,
        Error["Cannot find ADM mass in standard output of run "<>runName]]];
      ToExpression@Last@StringSplit[lines[[1]]]]];


ReadPunctureADMMassesFromFiles[files_List] :=
  Module[{lines, massLines, file, plusLine, minusLine, mPlus, mMinus},
    If[files === {}, Error["Cannot find puncture ADM masses"]];
    file = files[[1]];
    lines = ReadList[file, String];
    massLines = Select[lines, StringMatchQ[#, "INFO (TwoPunctures):   M_adm_" ~~ _ ~~ " = " ~~ __] &];
    If[massLines === {}, Return[ReadPunctureADMMassesFromFiles[Drop[files, 1]]]];

    plusLine = Select[massLines, StringMatchQ[#, "INFO (TwoPunctures):   M_adm_+ = " ~~ __] &][[1]];
    minusLine = Select[massLines, StringMatchQ[#, "INFO (TwoPunctures):   M_adm_- = " ~~ __] &][[1]];

    mPlus  = ToExpression[First@StringCases[plusLine, "INFO (TwoPunctures):   M_adm_+ = " ~~ x__ -> x]];
    mMinus = ToExpression[First@StringCases[minusLine, "INFO (TwoPunctures):   M_adm_- = " ~~ x__ -> x]];

    Return[{mPlus, mMinus}];
    ];

DefineMemoFunction[ReadPunctureADMMasses[run_String],
  Module[{stdoutFiles},
    stdoutFiles = StandardOutputOfRun[run];
    ReadPunctureADMMassesFromFiles[stdoutFiles]]];

DefineMemoFunction[ReadPunctureADMMassParameters[run_String],
  ToExpression/@{LookupParameter[run, "TwoPunctures::target_M_plus"], 
   LookupParameter[run, "TwoPunctures::target_M_minus"]}];



(* Read data output by the standalone TwoPunctures code by Marcus Ansorg *)
ReadTwoPuncturesData[file_String, col_] :=
 Module[{lines, lines2, table},
  lines = Import[file, "Table"];
  lines2 = Drop[Select[lines, Length[#] != 0 && First[#] != "#" &], 0];
  table = Map[Append[Take[#, 3], #[[col]]] &, lines2];
  TableToDataRegion[table]];

(* Initial data *)

TotalMass[run_] :=
 Plus @@ ReadPunctureADMMassParameters[run];

DefineMemoFunction[InitialSpinAngularMomentum[run_],
 Module[{sp, sm},
  sp = Table[
     ToExpression@
      LookupParameter[run, 
       "TwoPunctures::par_s_plus[" <> ToString[i] <> "]", "0"], {i, 0,
       2}];
  sm = Table[
     ToExpression@
      LookupParameter[run, 
       "TwoPunctures::par_s_minus[" <> ToString[i] <> "]", "0"], {i, 
      0, 2}];
  sp + sm]];

InitialLinearMomentum[run_, idx_] :=
  Module[{suffix},
    suffix = If[idx == 0, "plus", "minus"];
    Table[
      ToExpression@
        LookupParameter[run, "TwoPunctures::par_P_"<>suffix<>"["<>ToString@d<>"]", 0],
      {d,0,2}]];

InitialOrbitalAngularMomentum[run_] :=
 Module[{xp, xm, pyp, pym, initialL},
  xp = (ToExpression@LookupParameter[run, "TwoPunctures::par_b"] + 
      ToExpression@
       LookupParameter[run, "TwoPunctures::center_offset[0]"]);
  xm = (-ToExpression@LookupParameter[run, "TwoPunctures::par_b"] + 
      ToExpression@
       LookupParameter[run, "TwoPunctures::center_offset[0]"]);
  pyp = (ToExpression@
      LookupParameter[run, "TwoPunctures::par_P_plus[1]"]);
  pym = (ToExpression@
      LookupParameter[run, "TwoPunctures::par_P_minus[1]"]);
  initialL = {0, 0, xp*pyp + xm*pym}
  ];

InitialAngularMomentum[run_] :=
  InitialOrbitalAngularMomentum[run] + InitialSpinAngularMomentum[run];


MassRatio[run_] :=
 Module[{mp, mm},
  {mp, mm} = ReadPunctureADMMassParameters[run];
  Return[If[mp < mm, mp/mm, mm/mp]]];

SymmetricMassRatio[run_] :=
 Module[{q = MassRatio[run]},
  q/(1 + q)^2];


End[];

EndPackage[];
