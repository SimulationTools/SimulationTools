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

BeginPackage["SimulationTools`TwoPunctures`",
 {
  "SimulationTools`DataRegion`",
  "SimulationTools`Error`",
  "SimulationTools`IniFile`",
  "SimulationTools`Memo`",
  "SimulationTools`Parameters`",
  "SimulationTools`RunFiles`",
  "SimulationTools`SimulationProperties`"
 }];

ReadPunctureADMMasses(*::usage = "ReadPunctureADMMasses[sim] reads the ADM masses of the punctures in sim as computed by the TwoPunctures thorn."*);
ReadPunctureADMMassParameters(*::usage  = "ReadPunctureADMMassParameters[sim] reads the ADM masses of the punctures in sim as requested by the target_M_plus and target_M_minus parameters of the TwoPunctures thorn."*);
ReadPunctureBareMassParameters(*::usage  = "ReadPunctureBareMassParameters[sim] reads the bare masses of the punctures in sim as requested by the par_m_plus and par_m_minus parameters of the TwoPunctures thorn."*);
ReadPunctureSpinParameters;
TotalMass;
InitialSpinAngularMomentum;
InitialLinearMomentum;
InitialOrbitalAngularMomentum;
InitialAngularMomentum;

MassRatio;
SymmetricMassRatio;
ReadTwoPuncturesData(*::usage = "ReadTwoPuncturesData[file, col] reads a data file output by the standalone TwoPunctures code by Marcus Ansorg and returns a DataRegion containing the data in column col. col can be 1, 2 or 3 for the coordinates or >= 4 for the data."*);

Begin["`Private`"];

SimulationTools`TwoPunctures`InitialData`HaveData[run_String, ___] :=
  HaveRunDir[run] && (
    FindSimulationFiles[run, "TwoPunctures.bbh"] =!= {} ||
    FindSimulationFiles[run, "ADM_mass_tot.asc"] =!= {} ||
    StandardOutputOfRun[run] =!= {});

SimulationTools`TwoPunctures`InitialData`ReadADMMass[runName_String] :=
  Module[{massMDFiles, output, lines},

    massMDFiles = FindSimulationFiles[runName, "TwoPunctures.bbh"];
    If[massMDFiles =!= {},
      Return[ToExpression@IniVariable[massMDFiles[[1]], "initial-ADM-energy"]]];
    
    massMDFiles = FindSimulationFiles[runName, "ADM_mass_tot.asc"];
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

DefineMemoFunction[ReadPunctureBareMassParameters[run_String],
 Module[{masses, stdout, lines, mp, mm},

  If[LookupParameter[run, "TwoPunctures::give_bare_mass"] != "no",
    (* First try the parameter file *)
    masses = ToExpression/@{LookupParameter[run, "TwoPunctures::par_m_plus"],
                            LookupParameter[run, "TwoPunctures::par_m_minus"]};
  ,
    (* If the bare masses were not given explicitly, then search stdout *)
    stdout = StandardOutputOfRun[run];

    If[Length[stdout] < 1,
      Error["Cannot find standard output for run "<>run]];

    (* New-style standard output *)
    lines = FindList[First[stdout], "The two puncture masses are", 1];
    masses = StringCases[lines,
       "mp=" ~~ mp : NumberString ~~ ___ ~~ "mm=" ~~ mm : NumberString :>
        ToExpression /@ {mp, mm}];

    (* Old-style standard output *)
    If[Dimensions[masses] != {1,1,2},
      lines = FindList[First[stdout], "bare mass: mp="];
      masses = StringCases[lines,
         "mp=" ~~ mp : NumberString ~~ ___ ~~ "mm=" ~~ mm : NumberString :>
          ToExpression /@ {mp, mm}];
    ];

    If[!MatchQ[Dimensions[masses], {_Integer,1,2}],
      Error["Cannot determine bare masses from standard output of run "<>run]];

    (*  Need to take the last matched bare mass lines in case old-style output was used *)
    masses = masses[[-1,1]];
  ];
  masses
 ]
];

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

ReadPunctureSpinParameters[run_, idx_] := Module[{suffix},
  suffix = If[idx == 0, "plus", "minus"];
  Table[
    ToExpression@
      LookupParameter[run, "TwoPunctures::par_s_"<>suffix<>"[" <> ToString[i] <> "]", "0"],
    {i, 0, 2}]
];

DefineMemoFunction[InitialSpinAngularMomentum[run_],
 Module[{sp, sm},
  sp = ReadPunctureSpinParameters[run, 0];
  sm = ReadPunctureSpinParameters[run, 1];
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
