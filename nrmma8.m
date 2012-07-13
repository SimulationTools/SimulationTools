(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)
(* Functions which are only compatible with Mathematica 8 or later *)

BeginPackage["nrmma8`",
 {
  "DataRegion`",
  "DataRepresentations`",
  "DataTable`"
 }];

FilterWavelet::usage = "FilterWavelet[data] filters 'data' using a discrete wavelet transform and making use of the SURELevel thresholding."
WaveletType::usage = "WaveletType is an option for FilterWavelet which specifies the type of wavelet. The default is DaubechiesWavelet[4]."
WaveletRefinementLevels::usage = "WaveletRefinementLevels is an option for FilterWavelet which specefies the number of refinement levels to use. The default is 6."
WaveletThreshold::usage = "WaveletThreshold is an option for FilterWavelet which sets the thresholding specification. The default is {\"SURELevel\"}." 

Begin["`Private`"];

Options[FilterWavelet] = {WaveletType->DaubechiesWavelet[4], WaveletRefinementLevels->6, WaveletThreshold->{"SURELevel"}};

tableRange[t_List, {tStart_?NumberQ, tEnd_?NumberQ}] :=
  Select[t, 
   (#[[1]] >= tStart && #[[1]] < tEnd) &];

partitionTable[t_List, {tMin_?NumberQ, tMax_?NumberQ}] :=
 Module[{before, middle, after},
  before = tableRange[t, {First[t][[1]], tMin}];
  middle = tableRange[t, {tMin, tMax}];
  after = tableRange[t, {tMax, Last[t][[1]] + 1}];
  Return[{before, middle, after}]
  ];

FilterWavelet[dt_DataTable, range_:All, opts___] :=
Module[{data, times, filtereddata, before, inrange, after},
  If[range === All,
    {before,inrange,after} = {{}, ToList[dt], {}},
    {before,inrange,after} = partitionTable[ToList[dt], range]];

  times = First /@ inrange;
  data = Last /@ inrange;

  filtereddata = FilterWavelet[data, opts];
  MakeDataTable[Join[before, Thread[{times,filtereddata}], after]]
]

FilterWavelet[DataRegion[h_, data_], opts___] := DataRegion[h, FilterWavelet[data, opts]];

FilterWavelet[data_List, OptionsPattern[]] := Module[{dwt, filtereddata},
  dwt = DiscreteWaveletTransform[data, OptionValue[WaveletType], OptionValue[WaveletRefinementLevels]];
  filtereddata = InverseWaveletTransform[WaveletThreshold[dwt, OptionValue[WaveletThreshold]]];
   
  filtereddata
]

End[];

EndPackage[];
