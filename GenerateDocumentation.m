<< nrmma`;

<< ApplicationMaker`;

packages =
 {"Ascii1D",
  "Ascii",
  "BHCoordinates",
  "CarpetHDF5",
  "CoordinateTransformations",
  "Convergence",
  "DataAnalysis",
  "DataRegion",
  "DataTable",
  "FieldLines",
  "Grids",
  "Horizons",
  "Kicks",
  "Memo",
  "NR",
  "NRExport",
  "nrmmaVersion",
  "Parameters",
  "Plotting",
  "Profile",
  "ReadHDF5",
  "RunFiles",
  "SimView",
  "SimViewRRMHD",
  "SystemStatistics",
  "Tracks",
  "TwoPunctures",
  "Waveforms"};

documentedSymbols[package_String] :=
  Select[Names[package<>"`*"], StringQ[ToExpression[#<>"::usage"]] &];

packageSymbols = Map[# -> documentedSymbols[#] &, packages];

workbenchSymbols =
  ToString/@{AsciiDataOfIndex,
   AsciiTimeOfIndex,
   ConvergenceMultiplier,
   ConvergenceRate,
   FindParameters,
   ListLinePlotWithLegend,
   LookupParameter,
   PresentationListLinePlot,
   ReadCPUHours,
   ReadCarpetASCII1D,
   ReadCores,
   ReadMemory,
   ReadRunSpeed,
   ReadSwap,
   ReadWalltime,
   RunDirectory,
   SimView};

docPackage[package_ -> symbols_] :=
  Map[CreateReferencePages["nrmma", FileNameJoin[{$UserBaseDirectory, "Applications"}],
                           package, #, ReferenceTemplate -> False] &, Complement[Take[symbols,Min[Infinity,Length[symbols]]], workbenchSymbols]];

Scan[docPackage, packageSymbols];

OverviewGuide["nrmma", packageSymbols];

BuildApplication["nrmma", CreatePacletInfo -> False];
