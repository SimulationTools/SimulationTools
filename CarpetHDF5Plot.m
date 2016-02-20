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

BeginPackage["SimulationTools`CarpetHDF5Plot`",
 {
  "SimulationTools`CarpetHDF5`",
  "SimulationTools`DataRegion`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Plotting`",
  "SimulationTools`RunFiles`"
 }];

Carpet1DPlotter(*::usage = "Carpet1DPlotter[sim] generates an interactive plot of the 1D HDF5 variables in sim"*);

(* To Do:

  * Make the slider optionally work in terms of time instead of
    iteration.  In this case we would look for the largest iteration
    which have a time lower than the given slider value.

  * Display multiple variables at the same time (requires time slider)

  * Generalise to 2D plots

*)

Begin["`Private`"];

varsInRun[run_String] :=
 Module[{files},
  files = 
   Map[FileNameTake[#,-1]&, FindSimulationFiles[run, "*.x.h5"]];
  Map[StringReplace[#, ".x.h5" -> ""] &, files]];

fileOfRunVar[run_String, var_String] :=
 FindSimulationFiles[run, var <> ".x.h5"][[1]];

rlsInRunVar[run_String, var_String] :=
 CarpetHDF5RefinementLevels[fileOfRunVar[run, var]];

getIts[run_String, var_String, rl_Integer] :=
 Module[{its},
  its = CarpetHDF5Iterations[fileOfRunVar[run, var], rl];
  {First[its], Last[its], its[[2]] - its[[1]]}];

Options[plotVar] = {PlotOptions -> {}};

plotVar[run_String, var_String, rl_Integer, it_Integer, 
  opts : OptionsPattern[]] :=
 Module[{data},
  data = ToDataTable@
    ReadCarpetHDF5Variable[fileOfRunVar[run, var], Iteration -> it, 
     RefinementLevel -> rl];
  DynamicListLinePlot[data, 
   PlotLabel -> 
    var <> " at t = " <> ToString[ReadAttribute[data, "Time"]] <> 
     " on rl " <> ToString[rl] <> " in run " <> run, 
   ImageSize -> Full, Sequence @@ OptionValue[PlotOptions]]];

Options[Carpet1DPlotter] = Options[plotVar];
Carpet1DPlotter[run_, opts:OptionsPattern[]] :=
  DynamicModule[{var, rl, it, min = 0, max = 1, auto = True},
   Panel@Column[
     {Dynamic@InputField[Dynamic@run, String],
      Dynamic[PopupMenu[Dynamic@var, varsInRun[run]], 
       TrackedSymbols :> {run}],
      Dynamic@SetterBar[Dynamic@rl, rlsInRunVar[run, var]],
      Dynamic@Slider[Dynamic@it, getIts[run, var, rl]],
      Row[{Labeled[InputField[Dynamic[min], Number, FieldSize -> 4], 
         "Min"],
        Labeled[InputField[Dynamic[max], Number, FieldSize -> 4], 
         "Max"], Labeled[Checkbox[Dynamic[auto]], "Automatic"]}],
      Framed[
       Dynamic@plotVar[run, var, rl, it, 
         PlotOptions -> {Sequence@@OptionValue[PlotOptions],PlotRange ->
            If[auto, Automatic, {min, max}]}], Background -> White, 
       ImageSize -> {350, 260}]}]];

End[];

EndPackage[];
