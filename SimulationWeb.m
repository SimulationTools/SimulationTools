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
       Export[outDir <> "/" <> a["Filename"] <> ".png", 
         Labeled[Show[a["Plot"], PlotLabel -> None], 
           Style[a["Title"], FontFamily -> "Sans", 16, 
             FontWeight -> "Bold"], Top]]]],
   {fn, plotFunctions}]];

End[];
EndPackage[];
