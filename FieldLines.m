(* ::Package:: *)

(* Copyright 2010-2012 Kyriaki Dionysopoulou

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

BeginPackage["SimulationTools`FieldLines`",
 {
  "SimulationTools`GridFunctions`",
  "SimulationTools`DataAnalysis`",
  "SimulationTools`DataRegion`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Parameters`",
  "SimulationTools`Plotting`",
  "SimulationTools`Profile`",
  "SimulationTools`RunFiles`"
 }];

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

FieldLinesData2d::usage = "FieldLinesData2d[sim,field,it,rfl,plane] "<>
	"(EXPERIMENTAL)"<>
	"Calculates the fieldlines of 2D data of a field B, E, v etc. "<>
	"Plane takes values from 0 to 2.";
FieldLinesData3d::usage = "FieldLinesData3d[sim,field,it,rfl,plane] "<>
	"(EXPERIMENTAL)"<>
	"Calculates the fieldlines of 3D data of a field B, E, v etc. "<>
	"Plane takes values from 0 to 2.";
FieldLines2dPlot::usage = "FieldLines2dPlot[sim,field,it,rfl,plane,function2,opts] "<>
	"(EXPERIMENTAL)"<>
	"Plots the fieldlines and the density plot of 2D data of a field B, E, v";
FieldLines3dPlot::usage = "FieldLines3dPlot[sim,field,it,rfl,plane,function2,opts] "<>
	"(EXPERIMENTAL)"<>
	"Plots the fieldlines and the density plot of 3D data of a field B, E, v. "<>
	"Plane takes values from 0 to 2.";
MovieFieldLines2d::usage = "MovieFieldLines2d[simlist,field,rfl,plane,function,opts] "<>
	"(EXPERIMENTAL)"<>
	"Create a movie of the field lines of a 2D variable with fixed color range. "<>
	"Plane takes values from 0 to 2.";
MovieFieldLines3d::usage = "MovieFieldLines3d[simlist,field,rfl,plane,function,opts] "<>
	"(EXPERIMENTAL)"<>
	"Create a movie of the field lines of a 3D variable with fixed color range. "<>
	"Plane takes values from 0 to 2.";
SequenceFieldLines2d::usage = "SequenceFieldLines2d[path,simlist,field,rfl,plane,function,opts] "<>
	"(EXPERIMENTAL)"<>
	"Create a sequence of figures of a 2D variable with its field lines with a fixed "<>
	"color range. Plane takes values from 0 to 2.";
SequenceFieldLines3d::usage = "SequenceFieldLines3d[path,simlist,field,rfl,plane,function,xmin1,xmax1,opts] "<>
	"(EXPERIMENTAL)"<>
	"Create a sequence of figures of a 3D variable with its field lines with a fixed "<>
	"color range. Plane takes values from 0 to 2.";
Movie2d::usage = "Movie2d[simlist,var,rfl,function,opts] "<>
	"(EXPERIMENTAL)"<>
	"Create a movie of a 2D variable with fixed color range.";
Sequence2d::usage = "Sequence2d[path,simlist,var,rfl,function,xmin,xmax,opts] "<>
	"(EXPERIMENTAL)"<>
	"Create a sequence of figures of a 2D variable with fixed color range.";
ColorRanges2d::usage = "ColorRanges2d[simlist,var,rfl,func] "<>
	"(EXPERIMENTAL)"<>
	"Calculate a list of the universal Minimum and Maximum among 2D data of several "<>
	"simulations. ColorRange can be used in order to set a universal ColorBar in movies.";
ColorRanges3d::usage = "ColorRanges3d[simlist,var,plane,rfl,func] "<>
	"(EXPERIMENTAL)"<>
	"Calculate a list of the universal Minimum and Maximum among 3D data of several "<>
	"simulations. ColorRange can be used in order to set a universal ColorBar in movies. "<>
	"Plane takes values from 0 to 2.";
ColorRanges;
ReadIterationsFromRun::usage = "ReadIterationsFromRun[simlist,var,rfl] "<>
	"(EXPERIMENTAL)"<>
	"Read Iteration List from a list of simulations";

Begin["`Private`"];



fontname="Lucida";
fontsize=Directive[Thick,FontFamily-> fontname,14];
basesize=Directive[FontFamily-> fontname,14];
keyfontsize=Directive[FontFamily-> fontname,12];

MsolarInms=SolarMassInSeconds 1000;

downsample[d_,every_]:=Join[{d[[1]]},Select[d,Flatten[Mod[Position[d,#],every]][[1]]==0&]];

data[it_,var_,run_,rfl_]:=Map[ReadGridFunction[#,var,it,rfl]&,run];
minits[run_,var_,rfl_]:=Flatten[Position[Map[Max[ReadIterations[#,var,rfl]]&,run],Min[Map[Max[ReadIterations[#,var,rfl]]&,run]]]][[1]];

datait[runlist_,var_,it_,rfl_,sliceplane___]:=
	If[ StringSplit[var,"."][[2]]!="file_0",
		Map[GetData[ReadGridFunction[#,var,it,rfl,StripGhostZones->True]]&,runlist],
		Map[GetData[SliceData[ReadGridFunction[#,var,it,rfl,StripGhostZones->True],sliceplane]]&,runlist]];

ReadIterationsFromRun[runlist_,var_,rfl_]:=Range[Min[Map[Min[ReadIterations[#,var,rfl]]&,runlist]],Min[Map[Max[ReadIterations[#,var,rfl]]&,runlist]],
												  Max[Map[ToExpression[If[StringSplit[var,"."][[2]]!= "file_0",
																			LookupParameter[#,"CarpetIOHDF5::out2D_every"],
																			LookupParameter[#,"CarpetIOHDF5::out_every"]]]&,runlist]]];
(*ColorRanges[runlist_,var_]:={Min[Map[Log10@Abs@datait[runlist,var,#]&,its[runlist]]],Max[Map[Log10@Abs@datait[runlist,var,#]&,its[runlist]]]}*)
ColorRanges2d[runlist_,var_,rfl_,func_]:=With[{data=Map[datait[runlist,var,#,rfl]&,ReadIterationsFromRun[runlist,var,rfl]]},{Min[func[data]],Max[func[data]]}]
(*ColorRanges[runlist_,var_]:={Min[Map[Log10[Abs[datait[runlist,var,#]]+10^-20]&,its[runlist]]],Max[Map[Log10[Abs[datait[runlist,var,#]]+10^-20]&,its[runlist]]]}*)
ColorRanges3d[runlist_,var_,sliceplane_,rfl_,func_]:=With[{data=Map[datait[runlist,var,#,rfl,sliceplane]&,ReadIterationsFromRun[runlist,var,rfl]]},{Min[func[data]],Max[func[data]]}]
(*ColorRanges[runlist_,var_]:={Min[Map[Log10[Abs[datait[runlist,var,#]]+10^-20]&,its[runlist]]],Max[Map[Log10[Abs[datait[runlist,var,#]]+10^-20]&,its[runlist]]]}*)


FieldLinesData2d[run_,field_,it_,rfl_,plane_,unit_]:=
Module[{field1,field2,fieldint1,fieldint2,fieldlines,x1Min,x2Min,x1Max,x2Max},
Catch[If[plane===1,
    field1=ReadGridFunction[run,StringJoin[field,"x.xz.h5"],it,rfl,StripGhostZones->True];
         field2=ReadGridFunction[run,StringJoin[field,"z.xz.h5"],it,rfl,StripGhostZones->True];,
     If[plane === 2,
         field1=ReadGridFunction[run,StringJoin[field,"x.xy.h5"],it,rfl,StripGhostZones->True];
                field2=ReadGridFunction[run,StringJoin[field,"y.xy.h5"],it,rfl,StripGhostZones->True];,
         If[plane === 0,
             field1=ReadGridFunction[run,StringJoin[field,"y.yz.h5"],it,rfl,StripGhostZones->True];
                       field2=ReadGridFunction[run,StringJoin[field,"z.yz.h5"],it,rfl,StripGhostZones->True];,
	    Error["Unrecognized plane option " <>plane]]]];
fieldint1=Interpolation[field1];
  fieldint2=Interpolation[field2];
  {{x1Min,x1Max},{x2Min,x2Max}}=CoordinateRanges[field1];
fieldlines=Table[{{x1 unit,x2 unit},{fieldint1[x1,x2],fieldint2[x1,x2]}},{x1,x1Min,x1Max,CoordinateSpacings[field1][[1]]},{x2,x2Min,x2Max,CoordinateSpacings[field1][[2]]}]
]];

FieldLinesData3d[run_,field_,it_,rfl_,plane_]:=
Module[{field1,field2,fieldint1,fieldint2,fieldlines,x1Min,x2Min,x1Max,x2Max},
Catch[If[plane===1,
    field1=SliceData[ReadGridFunction[run,field<>"x.file_0.h5",it,rfl,StripGhostZones->True],2];
         field2=SliceData[ReadGridFunction[run,field<>"z.file_0.h5",it,rfl,StripGhostZones->True],2];,
     If[plane === 2,
         field1=SliceData[ReadGridFunction[run,field<>"x.file_0.h5",it,rfl,StripGhostZones->True],3];
                field2=SliceData[ReadGridFunction[run,field<>"y.file_0.h5",it,rfl,StripGhostZones->True],3];,
         If[plane === 0,
             field1=SliceData[ReadGridFunction[run,field<>"y.file_0.h5",it,rfl,StripGhostZones->True],1];
                       field2=SliceData[ReadGridFunction[run,field<>"z.file_0.h5",it,rfl,StripGhostZones->True],1];,
	    Error["Unrecognized plane option " <>plane]]]];
fieldint1=Interpolation[field1];
  fieldint2=Interpolation[field2];
  {{x1Min,x1Max},{x2Min,x2Max}}=CoordinateRanges[field1];
fieldlines=Table[{fieldint1[x1,x2],fieldint2[x1,x2]},{x1,x1Min,x1Max,CoordinateSpacings[field1][[1]]},{x2,x2Min,x2Max,CoordinateSpacings[field1][[2]]}]
]]

key[mapName_String, {min_, max_}, plotkeysize_,opts___] :=
  ArrayPlot[Table[{c, c}, {c, max, min, -(max - min)/100}],
   opts,
   ColorFunctionScaling -> False,
   ColorFunction ->
    ScaledColorFunction[mapName, {min, max}],FrameStyle->keyfontsize,
   DataRange -> {{0, 1}, {min, max}},AspectRatio->8, ImageSize-> plotkeysize,
   FrameTicks -> {Table[
      If[Abs[c] < 10^-15, 0, N@c], {c, min, max, (max - min)/10}],
     False, False, False}]

Options[FieldLines2dPlot] = Options[ArrayPlot]~Join~Options[ListStreamPlot]~Join~{ColorRanges->Automatic,PlotVariable->"rho.xz.h5",Units->"Normal"};
(*SetOptions[newplot,ImageSize->300];*)
 FieldLines2dPlot[run_,field_,it_,rfl_,plane_,function_,opts:OptionsPattern[]]:=
Catch[Module[{planestr,(*runstr,*)data,ndims,dataRange,plot1,plot2,plotRange,imageSize,units},
planestr=If[plane===0,"yz",If[plane===1,"xz","xy"]];
(*runstr=If[Length[run]>20,StringTake[run,-20],run];*)
data=ReadGridFunction[run,OptionValue[FieldLines2dPlot,PlotVariable],it,rfl,StripGhostZones->True];
ndims = GetNumDimensions[data];
If[ToString[Head@OptionValue[FieldLines2dPlot,Units]]==="Real",
		units=OptionValue[FieldLines2dPlot,Units],
		If[ToString@OptionValue[FieldLines2dPlot,Units]==="Normal",units=1,
		Throw["OptionValue Units not recognized"]]];
dataRange=If[ndims==1, CoordinateRanges[data][[1]],CoordinateRanges[data]] units;
plotRange=If[ToString[OptionValue[FieldLines2dPlot,PlotRange]]==="All",dataRange,OptionValue[FieldLines2dPlot,PlotRange]];
imageSize=If[ToString[OptionValue[FieldLines2dPlot,ImageSize]]==="Automatic",300,OptionValue[FieldLines2dPlot,ImageSize]];
data = GetData[data];
data = Reverse[data];
plot1=ArrayPlot[function[data],PlotRange->plotRange,ImageSize->imageSize,DataRange->dataRange,FilterRules[{opts},Options[ArrayPlot]],
							ColorFunctionScaling->False,Frame-> {{True,True},{True,True}},Axes->True,
							FrameTicks->True,LabelStyle->Medium,
							FrameLabel->{{StringTake[planestr,1],""},{StringTake[planestr,-1],""}}];
plot2=ListStreamPlot[FieldLinesData2d[run,field,it,rfl,plane,units],
					DataRange->dataRange,PlotRange->plotRange,ImageSize->imageSize,FilterRules[{opts},Options[ListStreamPlot]],
					StreamPoints->Coarse,Mesh->10,StreamScale->None,
					StreamStyle->Directive[Black,Thick], 
					FrameLabel-> {{StringTake[planestr,1],""},{StringTake[planestr,-1],""}},
					FrameStyle->fontsize];
Show[{plot1,plot2},BaseStyle-> basesize,FrameStyle->fontsize,
				PlotLabel-> Style[(*If[Length[FindParameters[run, "whisky::whisky_Rmhd_on"]] == 1, 
															If[StringCases[LookupParameter[run, "whisky::whisky_Rmhd_on"],"yes"] == {"yes"},
																"R_","I_"],
															"I_"]<>*)If[StringLength[run]>20,StringTake[run,-20],run],
				FontFamily->fontname,Bold,14]]]]


Options[FieldLines3dPlot] = Options[ArrayPlot]~Join~Options[ListStreamPlot];
(*SetOptions[newplot,ImageSize->300];*)

 FieldLines3dPlot[run_,field_,it_,rfl_,plane_,function_,opts:OptionsPattern[]]:=
Catch[Module[{planestr,(*runstr,*)data,ndims,dataRange,plot1,plot2,plotRange,imageSize},
planestr=If[plane===0,"yz",If[plane===1,"xz","xy"]];
(*runstr=If[Length[run]>20,StringTake[run,-20],run];*)
data=SliceData[ReadGridFunction[run,StringJoin[field,"_norm.file_0.h5"],it,rfl,StripGhostZones->True],plane];
ndims = GetNumDimensions[data];
dataRange=If[ndims==1, CoordinateRanges[data][[1]],CoordinateRanges[data]];
plotRange=If[ToString[OptionValue[FieldLines3dPlot,PlotRange]]==="All",dataRange,OptionValue[FieldLines3dPlot,PlotRange]];
imageSize=If[ToString[OptionValue[FieldLines3dPlot,ImageSize]]==="Automatic",300,OptionValue[FieldLines3dPlot,ImageSize]];
data = GetData[data];
data = Reverse[data];
plot1 = ArrayPlot[function[data],PlotRange->plotRange,ImageSize->imageSize,DataRange->dataRange,FilterRules[{opts},Options[ArrayPlot]],
							ColorFunctionScaling->False,Frame-> {{True,True},{True,True}},Axes->True,
							FrameTicks->True,LabelStyle->Medium,
							FrameLabel->{{StringTake[planestr,1],""},{StringTake[planestr,-1],""}}];
plot2 = ListStreamPlot[FieldLinesData3d[run,field,it,rfl,plane],
					DataRange->dataRange,PlotRange->plotRange,ImageSize->imageSize,FilterRules[{opts},Options[ListStreamPlot]],
					StreamPoints->Coarse,Mesh->10,StreamScale->None,
					StreamStyle->Directive[Black,Thick], 
					FrameLabel-> {{StringTake[planestr,1],""},{StringTake[planestr,-1],""}},
					FrameStyle->fontsize];
Show[{plot1,plot2},BaseStyle-> basesize,FrameStyle->fontsize,
				PlotLabel-> Style[(*If[Length[FindParameters[run, "whisky::whisky_Rmhd_on"]] == 1, 
															If[StringCases[LookupParameter[run, "whisky::whisky_Rmhd_on"],"yes"] == {"yes"},
																"R_","I_"],
															"I_"]<>*)If[StringLength[run]>20,StringTake[run,-20],run],
				FontFamily->fontname,Bold,14]]]]

(*Options[newplotdensity] = Options[ArrayPlot];
newplotdensity[it_,run_,var_,rfl_,func_,opts___]:=Catch[Module[{data,ndims,dataRange,plot1,plot2},
data=ReadGridFunction[run,StringJoin[var,".xz.h5"],it,rfl,StripGhostZones->True];
ndims = GetNumDimensions[data];
dataRange=If[ndims==1, CoordinateRanges[data][[1]],CoordinateRanges[data]];
data = GetData[data];
data = Reverse[data];
plot1=ArrayPlot[func@data,Frame-> {{True,True},{True,True}},Axes->True,FrameTicks->True,LabelStyle->Medium,DataRange->dataRange,PlotRange->dataRange,ColorFunctionScaling->False,BaseStyle-> basesize,FrameStyle->fontsize,FrameLabel->{{"x",""},{"z",""}},PlotLabel-> Style[If[Length[FindParameters[run, "whisky::whisky_Rmhd_on"]] == 1, If[StringCases[LookupParameter[run, "whisky::whisky_Rmhd_on"], 
        "yes"] == {"yes"}, 
      (* "RMHD code","Ideal-MHD code"],"Ideal-MHD code"],FontFamily->fontname,Bold,14],opts]]]*)
"R_"<>run,"I_"<>run],"I_"<>run],FontFamily->fontname,Bold,14],opts]]]
*)
Options[MovieFieldLines2d] = Options[ArrayPlot]~Join~Options[ListStreamPlot]~Join~{DownSample->1,ColorRanges->Automatic,PlotVariable->"rho.xz.h5",Units->Normal};

MovieFieldLines2d[run_,field_,rfl_,plane_,function2_,opts:OptionsPattern[]]:=
Catch[Module[{planestr,plotvariable,colorrange1,xmincolorrange1,xmaxcolorrange1,iterations,imageSize,xmin1,xmax1},
	planestr=If[plane===0,"yz",If[plane===1,"xz","xy"]];
	plotvariable=OptionValue[MovieFieldLines2d,PlotVariable];
	colorrange1=If[ToString[OptionValue[MovieFieldLines2d,ColorRanges]]==="Automatic", 
							ColorRanges2d[run,plotvariable,rfl,function2],
							OptionValue[MovieFieldLines2d,ColorRanges]];
	xmincolorrange1= Range[colorrange1[[1]],colorrange1[[2]],(colorrange1[[2]]-colorrange1[[1]])/100];
	xmaxcolorrange1 = Reverse[xmincolorrange1];
	imageSize=If[ToString[OptionValue[MovieFieldLines2d,ImageSize]]==="Automatic",300,OptionValue[MovieFieldLines2d,ImageSize]];
	iterations = downsample[ReadIterationsFromRun[{run[[minits[run,plotvariable,rfl]]]},plotvariable,rfl],OptionValue[MovieFieldLines2d,DownSample]];
	Manipulate[With[{keymag=key["TemperatureMap",{xmin1,xmax1},{Automatic,imageSize},AspectRatio->10],
		data=ReadTime[run[[minits[run,plotvariable,rfl]]],plotvariable,it,rfl,StripGhostZones->True]},
		Column[{Style["t = " <> ToString[data MsolarInms]<>" ms",FontFamily->fontname,Bold,20],
			Row[Append[Map[FieldLines2dPlot[#,field,it,rfl,plane,function2,ImageSize->imageSize,FilterRules[{opts},Options[FieldLines2dPlot]],ColorFunctionScaling->False,
							ColorFunction-> ScaledColorFunction["TemperatureMap",{xmin1,xmax1}]]&,run],keymag],
				"  ",
				ImageMargins-> {{2,2},{2,2}}]},Center,Spacings->1]],
		{it,iterations},{xmin1,xmincolorrange1},{xmax1,xmaxcolorrange1},
		ControlType->{Slider,PopupMenu,PopupMenu},SynchronousUpdating->False]]];

Options[MovieFieldLines3d] = Options[ArrayPlot]~Join~Options[ListStreamPlot];

MovieFieldLines3d[run_,field_,rfl_,plane_,function2_,opts:OptionsPattern[]]:=
Catch[Module[{planestr,colorrange1,xmincolorrange1,xmaxcolorrange1,iterations,imageSize,xmin1,xmax1},
	planestr=If[plane===0,"yz",If[plane===1,"xz","xy"]];
	colorrange1=ColorRanges3d[run,field<>"_norm."<>"file_0"<>".h5",plane,rfl,function2];
	xmincolorrange1= Range[colorrange1[[1]],colorrange1[[2]],(colorrange1[[2]]-colorrange1[[1]])/100];
	xmaxcolorrange1 = Reverse[xmincolorrange1];
	imageSize=If[ToString[OptionValue[MovieFieldLines3d,ImageSize]]==="Automatic",300,OptionValue[MovieFieldLines3d,ImageSize]];
	iterations = ReadIterationsFromRun[{run[[minits[run,field<>"_norm."<>"file_0"<>".h5",rfl]]]},field<>"_norm."<>"file_0"<>".h5",rfl];
	Manipulate[With[{keymag=key["TemperatureMap",{xmin1,xmax1},{Automatic,imageSize}],
		data=ReadTime[run[[minits[run,field<>"_norm."<>"file_0"<>".h5",rfl]]],field<>"_norm."<>"file_0"<>".h5",it,rfl,StripGhostZones->True]},
		Column[{Style["t = " <> ToString[data MsolarInms]<>" ms",FontFamily->fontname,Bold,20],
			Row[Append[Map[FieldLines3dPlot[#,field,it,rfl,plane,function2,ImageSize->imageSize,opts,ColorFunctionScaling->False,
							ColorFunction-> ScaledColorFunction["TemperatureMap",{xmin1,xmax1}]]&,run],keymag],
				"  ",
				ImageMargins-> {{2,2},{2,2}}]},Center,Spacings->1]],
		{it,iterations},{xmin1,xmincolorrange1},{xmax1,xmaxcolorrange1},
		ControlType->{Slider,PopupMenu,PopupMenu},SynchronousUpdating->False]]];

Options[SequenceFieldLines2d] = Options[ArrayPlot]~Join~Options[ListStreamPlot]~Join~{DownSample->1,PlotVariable->"rho.xz.h5",Units->Normal};
SequenceFieldLines2d[path_,run_,field_,rfl_,plane_,function2_,xmin1_,xmax1_,opts:OptionsPattern[]]:=
Catch[Module[{planestr,(*colorrange1,*)iterations,imageSize},
	planestr=If[plane===0,"yz",If[plane===1,"xz","xy"]];
	If[Not[DirectoryQ[DirectoryName[path]]],Print["The specified directory does not exist"]];
	If[Not[DirectoryQ[path]],CreateDirectory[path]];
	If[Not[DirectoryQ[path<>"/"<>field]],CreateDirectory[path<>"/"<>field]];
	If[Not[DirectoryQ[path<>"/"<>field<>"/"<>planestr]],CreateDirectory[path<>"/"<>field<>"/"<>planestr]];
	If[Not[DirectoryQ[path<>"/"<>field<>"/"<>planestr<>"/rfl"<>ToString[rfl]]],CreateDirectory[path<>"/"<>field<>"/"<>planestr<>"/rfl"<>ToString[rfl]]];
(*Print[OptionValue[movienew,ImageSize],OptionValue[movienew,PlotRange]];*)
(*	Print["ColorRanges: ",ColorRanges[run,field<>"_norm."<>plane<>".h5",rfl,function2]];*)
	imageSize=If[ToString[OptionValue[SequenceFieldLines2d,ImageSize]]==="Automatic",300,OptionValue[SequenceFieldLines2d,ImageSize]];
	iterations = downsample[ReadIterationsFromRun[{run[[minits[run,field<>"_norm."<>planestr<>".h5",rfl]]]},field<>"_norm."<>planestr<>".h5",rfl],OptionValue[SequenceFieldLines2d,DownSample]];
	Print[iterations];
	Do[Export[path<>"/"<>field<>"/"<>planestr<>"/rfl"<>ToString[rfl]<>"/"<>ToString[1000000+it]<>".png",With[{keymag=key["TemperatureMap",{xmin1,xmax1},{Automatic,imageSize}],
		data=ReadTime[run[[minits[run,field<>"_norm."<>planestr<>".h5",rfl]]],field<>"_norm."<>planestr<>".h5",it,rfl,StripGhostZones->True]},
		Column[{Style["t = " <> ToString[data MsolarInms]<>" ms",FontFamily->fontname,Bold,20],
			Row[Append[Map[FieldLines2dPlot[#,field,it,rfl,plane,function2,ImageSize->imageSize,FilterRules[{opts},Options[ArrayPlot]~Join~Options[ListStreamPlot]],ColorFunctionScaling->False,
							ColorFunction-> ScaledColorFunction["TemperatureMap",{xmin1,xmax1}]]&,run],keymag],
				"  ",
				ImageMargins-> {{2,2},{2,2}}]},Center,Spacings->1]],"PNG"],
		{it,iterations}]]];

Options[SequenceFieldLines3d] = Options[ArrayPlot]~Join~Options[ListStreamPlot];
SequenceFieldLines3d[path_,run_,field_,rfl_,plane_,function2_,xmin1_,xmax1_,opts:OptionsPattern[]]:=
Catch[Module[{(*colorrange1,*)planestr,iterations,imageSize},
(*Print[OptionValue[movienew,ImageSize],OptionValue[movienew,PlotRange]];*)
(*	Print["ColorRanges: ",ColorRanges[run,field<>"_norm."<>plane<>".h5",rfl,function2]];*)
	planestr=If[plane===0,"yz",If[plane===1,"xz","xy"]];
	If[Not[DirectoryQ[DirectoryName[path]]],Print["The specified directory does not exist"]];
	If[Not[DirectoryQ[path]],CreateDirectory[path]];
	If[Not[DirectoryQ[path<>"/"<>field]],CreateDirectory[path<>"/"<>field]];
	If[Not[DirectoryQ[path<>"/"<>field<>"/"<>planestr]],CreateDirectory[path<>"/"<>field<>"/"<>planestr]];
	imageSize=If[ToString[OptionValue[SequenceFieldLines3d,ImageSize]]==="Automatic",300,OptionValue[SequenceFieldLines3d,ImageSize]];
	iterations = ReadIterationsFromRun[{run[[minits[run,field<>"_norm."<>"file_0"<>".h5",rfl]]]},field<>"_norm."<>"file_0"<>".h5",rfl];
	Do[Export[path<>"/"<>field<>"/"<>planestr<>"/"<>ToString[1000000+it]<>".jpg",With[{keymag=key["TemperatureMap",{xmin1,xmax1},{Automatic,imageSize}],
		data=ReadTime[run[[minits[run,field<>"_norm."<>"file_0"<>".h5",rfl]]],field<>"_norm."<>"file_0"<>".h5",it,rfl,StripGhostZones->True]},
		Column[{Style["t = " <> ToString[data MsolarInms]<>" ms",FontFamily->fontname,Bold,20],
			Row[Append[Map[FieldLines3dPlot[#,field,it,rfl,plane,function2,ImageSize->imageSize,opts,ColorFunctionScaling->False,
							ColorFunction-> ScaledColorFunction["TemperatureMap",{xmin1,xmax1}]]&,run],keymag],
				"  ",
				ImageMargins-> {{2,2},{2,2}}]},Center,Spacings->1]]],
		{it,iterations}]]];

Options[Movie2d] = Options[ListDensityPlot]~Join~{"ColorRanges"->Automatic};
Movie2d[run_,var_,rfl_,function_,opts:OptionsPattern[]]:=
	Catch[Module[{colorrange,xmincolorrange,plane,data1,ndims,datarange,plotrange,imagesize,xmaxcolorrange,iterations,xmin,xmax},
		colorrange=If[ToString[OptionValue[Movie2d,ColorRanges]]==="Automatic", 
							ColorRanges2d[run,var,rfl,function],
							OptionValue[Movie2d,ColorRanges]];
		iterations=ReadIterationsFromRun[{run[[minits[run,var,rfl]]]},var,rfl];
		plane = If[StringSplit[var,"."][[2]]==="xz","xz",If[StringSplit[var,"."][[2]]==="yz","yz","xy"]];
		xmincolorrange = Sort[Append[Range[colorrange[[1]],colorrange[[2]],N[(colorrange[[2]]-colorrange[[1]])/100]],0]];
		xmaxcolorrange = Reverse[xmincolorrange];
		data1 = ReadGridFunction[run[[1]],var,iterations[[1]],rfl,StripGhostZones->True];
		ndims = GetNumDimensions[data1];
		datarange = If[ndims==1, CoordinateRanges[data1][[1]],CoordinateRanges[data1]];
		plotrange = If[OptionValue[Movie2d,PlotRange]==={Full,Full,Automatic},datarange,OptionValue[Movie2d,PlotRange]];
		imagesize = If[ToString[OptionValue[Movie2d,ImageSize]]==="Automatic",300,OptionValue[Movie2d,ImageSize]];
		Manipulate[With[{keymag=key["TemperatureMap",{xmin,xmax},
							{Automatic,imagesize}],
							data=ReadTime[run[[minits[run,var,rfl]]],var,it,rfl,StripGhostZones->True]},
						Column[{Style["t = " <> ToString[data MsolarInms]<>" ms",FontFamily->fontname,Bold,20],
							Row[Append[Map[ListDensityPlot[function[ReadGridFunction[#,var,it,rfl,StripGhostZones->True]],
											DataRange->datarange,PlotRange->Append[plotrange,All],ImageSize->imagesize,
											FilterRules[{opts},Options[ListDensityPlot]],
											Frame->{{True,True},{True,True}}, 
											Axes-> True,
											FrameLabel->{{StringTake[plane,-1],""},{StringTake[plane,1],
														(*If[Length[FindParameters[#, "whisky::whisky_Rmhd_on"]] == 1, 
															If[StringCases[LookupParameter[#, "whisky::whisky_Rmhd_on"],"yes"] == {"yes"},
																"R_","I_"],
															"I_"]
														<>*)If[StringLength[#]>20,StringTake[#,-20],#]}},
											FrameTicks->True,ColorFunctionScaling->False,ColorFunction->ScaledColorFunction["TemperatureMap",{xmin,xmax}],
											BaseStyle-> basesize,FrameStyle->fontsize,
											ClippingStyle->{Blue,Red},MeshFunctions->{#3&},
											Mesh->10]&,run],keymag],
										"  ",ImageMargins-> {{2,2},{2,2}}]},Center,Spacings->1]],
					{it,iterations},
					{xmin,xmincolorrange},
					{xmax,xmaxcolorrange},
					ControlType->{Slider,PopupMenu,PopupMenu},
					SynchronousUpdating->False]]];

Options[Sequence2d] = Options[ListDensityPlot]~Join~{"ColorRanges"->Automatic};
Sequence2d[path_,run_,var_,rfl_,function_,opts:OptionsPattern[]]:=
	Catch[Module[{(*colorrange,*)plane,data1,ndims,datarange,plotrange,imagesize,iterations,xmin,xmax},
(*		colorrange=ColorRanges[run,var,rfl,function];*)
(*		Print["ColorRanges: ",colorrange];*)
		iterations=ReadIterationsFromRun[{run[[minits[run,var,rfl]]]},var,rfl];
		plane = If[StringSplit[var,"."][[2]]==="xz","xz",If[StringSplit[var,"."][[2]]==="yz","yz","xy"]];
		If[Not[DirectoryQ[DirectoryName[path]]],Print["The specified directory does not exist"]];
		If[Not[DirectoryQ[path]],CreateDirectory[path]];
		If[Not[DirectoryQ[path<>"/"<>StringSplit[var,"."][[1]]]],CreateDirectory[path<>"/"<>StringSplit[var,"."][[1]]]];
		If[Not[DirectoryQ[path<>"/"<>StringSplit[var,"."][[1]]<>"/"<>plane]],
			CreateDirectory[path<>"/"<>StringSplit[var,"."][[1]]<>"/"<>plane]];
		If[Not[DirectoryQ[path<>"/"<>StringSplit[var,"."][[1]]<>"/"<>plane<>"/rfl"<>ToString[rfl]]],
			CreateDirectory[path<>"/"<>StringSplit[var,"."][[1]]<>"/"<>plane<>"/rfl"<>ToString[rfl]]];
		data1 = ReadGridFunction[run[[1]],var,iterations[[1]],rfl,StripGhostZones->True];
		ndims = GetNumDimensions[data1];
		datarange = If[ndims==1, CoordinateRanges[data1][[1]],CoordinateRanges[data1]];
		plotrange = If[OptionValue[Sequence2d,PlotRange]==={Full,Full,Automatic},datarange,OptionValue[Sequence2d,PlotRange]];
		imagesize = If[ToString[OptionValue[Sequence2d,ImageSize]]==="Automatic",300,OptionValue[Sequence2d,ImageSize]];
		{xmin,xmax}= (*{-9.66,*)If[ToString[OptionValue[Sequence2d,ColorRanges]]==="Automatic", 
							ColorRanges2d[run,var,rfl,function],
							OptionValue[Sequence2d,ColorRanges]];
		(*If[CreateDirectory["/home/kdionyso/Desktop/movies/"<>path]]*)
		Do[Export[path<>"/"<>StringSplit[var,"."][[1]]<>"/"<>plane<>"/rfl"<>ToString[rfl]<>"/"<>ToString[1000000+it]<>".png",With[{keymag=key["TemperatureMap",{xmin,xmax}(*[[2]]}*),
							{Automatic,imagesize}],
							magrange={xmin,xmax},
							data=ReadTime[run[[minits[run,var,rfl]]],var,it,rfl,StripGhostZones->True]},
						Column[{Style["t = " <> ToString[data MsolarInms]<>" ms",FontFamily->fontname,Bold,20],
							Row[Append[Map[ListDensityPlot[function[ReadGridFunction[#,var,it,rfl,StripGhostZones->True]],
											DataRange->datarange,PlotRange->Append[plotrange,All],ImageSize->imagesize,
											FilterRules[{opts},Options[ListDensityPlot]],Frame->{{True,True},{True,True}}, 
											Axes-> True,
											FrameLabel->{{StringTake[plane,-1],""},{StringTake[plane,1],
														(*If[Length[FindParameters[#, "whisky::whisky_Rmhd_on"]] == 1, 
															If[StringCases[LookupParameter[#, "whisky::whisky_Rmhd_on"],"yes"] == {"yes"},
																"R_","I_"],
															"I_"]
														<>*)If[StringLength[#]>20,StringTake[#,-20],#]}},
											FrameTicks->True,ColorFunctionScaling->False,
											BaseStyle-> basesize,FrameStyle->fontsize,
											ClippingStyle->{Blue,Red},MeshFunctions->{#3&},
											Mesh->10,
											ColorFunction-> ScaledColorFunction["TemperatureMap",magrange]]&,run],keymag],
										"  ",ImageMargins-> {{2,2},{2,2}}]},Center,Spacings->1]],"PNG"],
					{it,iterations}]]];




End[];

EndPackage[];
