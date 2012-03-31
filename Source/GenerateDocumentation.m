<< nrmma`;

<< DocumentationBuilder`;

RunDirectory = $NRMMATestRunDirectory;

packages =
{ 
  "Ascii",
  "Ascii1D",
  "AsyncCommand",
  "BHCoordinates",
  "CactusTimers",
  "CarpetHDF5Plot",
  "CarpetHDF5",
(*  "CarpetIOHDF5",*)
  "CircularPN",
  "Convergence",
  "CoordinateTransformations",
  "DataAnalysis",
  "DataRegion",
  "DataTable",
  "Empty",
  "Error",
  "FieldLines",
(*  "GridFunctions",*)
  "Grids",
  "Horizons",
  "IniFile",
  "InitialData",
  "Kicks",
  "Memo",
  "MinTracker",
  "Movie",
  "MultipoleASCII",
  "MultipoleHDF5",
  "NR",
  "NRDF",
  "NRExport",
  "nrmma8",
  "nrmmaVersion",
  "Parameters",
  "Plotting",
  "Profile",
  "Providers",
  "PunctureTracker",
  "Quasinormal",
  "ReadHDF5",
  "RunFiles",
  "ShiftTracker",
  "SimFactory",
  "SimView",
  "SimViewRRMHD",
  "Stack",
  "SystemStatistics",
  "Timers",
  "Tracks",
  "TwoPunctures",
  "VTK",
  "Waveforms",
  "YlmDecomp"
};

packageSymbols = Map[# -> DocumentedSymbols[#] &, packages];

undocumentedSymbols = Map[# -> UndocumentedSymbols[#] &, packages] /. (_ -> {}) -> Sequence[];
Map[Print["Undocumented symbols for package "<>#[[1]]<>" skipped:\n", #[[2]]]&, undocumentedSymbols];

Print["Building symbol reference pages"];
docPackage[package_ -> symbols_] :=
  Map[BuildSymbolReference["nrmma", #, "Source"] &, symbols];
Scan[docPackage, packageSymbols];

Print["Building guides"];
sourceGuides = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Guides"}], Infinity];
destGuides =
  FileNameJoin[{Directory[], FileNameDrop[DirectoryName[#], 1],
      FileBaseName[#] <> ".nb"}] & /@ sourceGuides;
MapThread[BuildGuide, {sourceGuides, destGuides}];

Print["Building tutorials"];
tutorialSources = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Tutorials"}], Infinity];
Map[BuildTutorial[FileNameJoin[{Directory[], #}]]&, tutorialSources];

Print["Indexing Documentation"];
BuildIndex["nrmma"];
