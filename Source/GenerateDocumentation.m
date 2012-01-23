<< nrmma`;

<< DocumentationBuilder`;

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
   ReadCarpetASCII1D};

docPackage[package_ -> symbols_] :=
  Map[BuildSymbolReference["nrmma", #, "Source"] &, Complement[Take[symbols,Min[Infinity,Length[symbols]]], workbenchSymbols]];

Print["Building symbol reference pages"];
Scan[docPackage, packageSymbols];

Print["Building guides"];
sourceGuides = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Guides"}], Infinity];
destGuides =
  FileNameJoin[{Directory[], FileNameDrop[DirectoryName[#], 1],
      FileBaseName[#] <> ".nb"}] & /@ sourceGuides;
MapThread[BuildGuide, {sourceGuides, destGuides}];

Print["Indexing Documentation"];
BuildIndex["nrmma"];
