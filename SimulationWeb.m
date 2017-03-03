(* Copyright 2010-2017 Ian Hinder and Barry Wardell

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

BeginPackage["SimulationTools`SimulationWeb`",
 {
  "SimulationTools`Error`"
 }];


ExportPlots;
ExportTables;

Begin["`Private`"];

ExportPlots[sims_List, plotFunctions_List, outDir_String] :=
 Module[{},
  Do[
   If[! FileExistsQ[outDir], CreateDirectory[outDir]];
   Module[{plot, a},
    plot = fn[sims];
    a = Replace[plot,
      {a_Association :> a,
       g:(_Graphics|_Legended) :> 
        Module[{title = 
           StringReplace[ToString[fn], "Plot" ~~ EndOfString -> ""]},
         Association["Plot" -> g, "Filename" -> title,
          "Title" -> title]],
        None :> None,
       x_ :> Error["Invalid value returned by " <> ToString[fn]<>": "<>ToString[Short[x]]]}];
    
     If[a =!= None,
       Print["Exporting ", a["Filename"] <> ".png"];
       Export[outDir <> "/" <> a["Filename"] <> ".png", 
         Labeled[Show[a["Plot"], PlotLabel -> None], 
           Style[a["Title"], FontFamily -> "Sans", 16, 
             FontWeight -> "Bold"], Top]]]],
   {fn, plotFunctions}]];

ExportTables[sims_List, tableFunctions_List, outDir_String] :=
 Module[{},
  Do[
   If[! FileExistsQ[outDir], CreateDirectory[outDir]];
   Module[{table, a},
    table = fn[sims];
    a = Replace[table,
      {a_Association :> a,
       d_Dataset :> 
        Module[{title = 
           StringReplace[ToString[fn], "Table" ~~ EndOfString -> ""]},
         Association["Table" -> d, "Filename" -> title,
          "Title" -> title]],
        None :> None,
       x_ :> Error["Invalid value returned by " <> ToString[fn]<>": "<>ToString[Short[x]]]}];
    
     If[a =!= None,
       Print["Exporting ", a["Filename"] <> ".html"];
       Export[outDir <> "/" <> a["Filename"] <> ".html", 
         XMLElement["div",{},
           {XMLElement["h2",{},{}],
             ToHTML[a["Table"]]}], "XML"]]],
   {fn, tableFunctions}]];

ToHTML[x_] := ToString[x]

ToHTML[d_Dataset] :=
 Module[{keys, row},
  keys = Keys[d[1]];
  row[l_, tag_String] :=
   
   XMLElement["tr", {}, Normal@Map[XMLElement[tag, {},
        {ToString[#]}] &, l]];
  XMLElement["table", {},
   Join[{row[keys, "th"]},
    row[#, "td"] & /@ Values /@ d]]];

End[];
EndPackage[];
