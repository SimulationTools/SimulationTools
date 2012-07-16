<< nrmma`;

<< ApplicationTools`;

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
  "CarpetIOHDF5",
  "CircularPN",
  "Convergence",
  "CoordinateTransformations",
  "DataAnalysis",
  "DataRegion",
  "DataRepresentations",
  "DataTable",
  "Empty",
  "Error",
  "FieldLines",
  "GridFunctions",
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

(**********************************************************************************************************)
(* HTML Documentation *)
(**********************************************************************************************************)

Print["Exporting HTML Documentation"];

Module[
  {exportNotebook, docDir,dest,tutorials,tutorialHTMLNames,tutorialTitles},

  exportNotebook[dest_String, nbf_String] :=
  Module[{nb},
         Print["  Exporting ", FileNameTake[nbf,-1]];
         nb = NotebookOpen[nbf];
         Export[dest<>"/" <> 
                StringReplace[FileNameTake[nbf, -1], ".nb" -> ".html"], nb];
         NotebookClose[nb]];

  docDir = FileNameJoin[{FileNameDrop[FindFile["nrmma`"], -2], "Documentation"}];

  dest = If[StringQ[$HTMLDestination], $HTMLDestination, "~/Sites/nrmma/examples"];

  tutorials = 
  Select[FileNames["*", FileNameJoin[{docDir, "English/Tutorials"}], 
                   Infinity], ! DirectoryQ[#] &];

  Print["Exporting tutorials"];

  Scan[exportNotebook[dest,#] &, tutorials];

  tutorialHTMLNames = 
  Map[StringReplace[FileNameTake[#, -1], ".nb" -> ".html"] &, 
      tutorials];

  tutorialTitles = {
    "DataRegion.html" -> "DataRegion", 
    "DataTable.html" -> "DataTable", 
    "GridFunctions.html" -> "Grid functions",
    "Kicks.html" -> "Kicks", 
    "NRMMAIntroduction.html" -> "Introduction", 
    "NumericalRelativity.html" -> "Numerical Relativity"};

  Print["Exporting tutorial index"];

  Export[dest<>"/list.html", 
         Map["<li><a href = \"examples/" <> # <> 
             "\">" <> (# /. tutorialTitles) <> "</a></li>" &, 
             tutorialHTMLNames], "Text"]];

Print["Done"];
