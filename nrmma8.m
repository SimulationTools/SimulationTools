(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)
(* Functions which are only compatible with Mathematica 8 or later *)

BeginPackage["nrmma8`", {"DataTable`", "DataRegion`"}];

FilterWavelet::usage = "FilterWavelet[data] filters 'data' using a discrete wavelet transform and making use of the SURELevel thresholding."
WaveletType::usage = "WaveletType is an option for FilterWavelet which specifies the type of wavelet. The default is DaubechiesWavelet[4]."
WaveletRefinementLevels::usage = "WaveletRefinementLevels is an option for FilterWavelet which specefies the number of refinement levels to use. The default is 6."
WaveletThreshold::usage = "WaveletThreshold is an option for FilterWavelet which sets the thresholding specification. The default is {\"SURELevel\"}." 

Begin["`Private`"];

Options[FilterWavelet] = {WaveletType->DaubechiesWavelet[4], WaveletRefinementLevels->6, WaveletThreshold->{"SURELevel"}};

FilterWavelet[dt_DataTable, opts___] := Module[{data, times, filtereddata},
  times = First /@ ToList[dt];
  data = Last /@ ToList[dt];
  filtereddata = FilterWavelet[data, opts];
  MakeDataTable[Thread[{times, filtereddata}]]
]

FilterWavelet[DataRegion[h_, data_], opts___] := DataRegion[h, FilterWavelet[data, opts]];

FilterWavelet[data_List, OptionsPattern[]] := Module[{dwt, filtereddata},
  dwt = DiscreteWaveletTransform[data, OptionValue[WaveletType], OptionValue[WaveletRefinementLevels]];
  filtereddata = InverseWaveletTransform[WaveletThreshold[dwt, OptionValue[WaveletThreshold]]];
   
  filtereddata
]

End[];

EndPackage[];
