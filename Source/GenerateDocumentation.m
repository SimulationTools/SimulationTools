<< nrmma`;

<< ApplicationTools`;

DataTable /: MakeBoxes[d_DataTable, StandardForm] =.;

Format[DataTable[l_List, attrs__]] :=
 DataTable[Row[{"<", Length[l], ">,", {{l[[1, 1]], l[[-1, 1]]}}}]];

$SimulationPath = {$NRMMATestSimulationDirectory};

(* BeginPackage["GenerateDocumentation`", {"ApplicationTools`"}]; *)

GenerateDocumentation[] :=
  Module[
    {packages,packageSymbols,undocumentedSymbols,docPackage,
     sourceGuides,
     destGuides,tutorialSources,docLink,generateHTMLDocumentation},

packages =
{ 
  (*"ArgumentChecker",*)
  "Ascii",
  "Ascii1D",
  "AsyncCommand",
  "BHCoordinates",
  (*"Binary",*)
  (*"BlackHole",*)
  "CactusTimers",
  "CarpetHDF5",
  "CarpetHDF5Plot",
  "CarpetIOHDF5",
  (*"CarpetVTK",*)
  "CircularPN",
  "Convergence",
  "CoordinateTransformations",
  "DataAnalysis",
  "DataRegion",
  "DataRepresentations",
  "DataTable",
  "Empty",
  "Error",
  (*"FieldLines",*)
  "GridFunctions",
  "Grids",
  "Horizons",
  "IniFile",
  "InitialData",
  "Kicks",
  "Memo",
  (*"MessageCatcher",*)
  "MinTracker",
  "Movie",
  "MultipoleASCII",
  "MultipoleHDF5",
  "NR",
  "NRDF",
  "NRExport",
  "Parameters",
  (*"Performance",*)
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
  (*"Statistics",*)
  "SystemStatistics",
  "Timers",
  (*"Trackers",*)
  "Tracks",
  "TwoPunctures",
  (*"Utils",*)
  "Waveforms",
  (*"Wavelets",*)
  "YlmDecomp"
};

packageSymbols = Map[# -> DocumentedSymbols[#] &, packages];

$PackageSymbols = packageSymbols; (* Used in the Overview.md file *)

undocumentedSymbols = Map[# -> UndocumentedSymbols[#] &, packages] /. (_ -> {}) -> Sequence[];
(* Map[Print["Undocumented symbols for package "<>#[[1]]<>" skipped:\n", #[[2]]]&, undocumentedSymbols]; *)

Print["Building symbol reference pages"];
docPackage[package_ -> symbols_] :=
  Map[(Print[#]; BuildSymbolReference["nrmma", #, "Source"]) &, symbols];
Scan[docPackage, packageSymbols];

Print["Building guides"];
sourceGuides = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Guides"}], Infinity];
destGuides =
  FileNameJoin[{Directory[], FileNameDrop[DirectoryName[#], 1],
      FileBaseName[#] <> ".nb"}] & /@ sourceGuides;
MapThread[BuildGuide, {sourceGuides, destGuides}];

Print["Building tutorials"];
tutorialSources = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Tutorials"}], Infinity];
Map[(Print[#]; BuildTutorial[FileNameJoin[{Directory[], #}]])&, tutorialSources];

Print["Indexing Documentation"];
BuildIndex["nrmma"];

(**********************************************************************************************************)
(* HTML Documentation *)
(**********************************************************************************************************)

Print["Exporting HTML Documentation"];

docLink[s_String] :=
 StringReplace[s,
  {"paclet:nrmma/ref/" ~~ ss__ :> 
    "/Documentation/English/ReferencePages/Symbols/" <> ss <> 
     ".xml",
   
   "paclet:nrmma/tutorial/" ~~ ss__ :> 
    "/Documentation/English/Tutorials/" <> StringReplace[ss," "->""] <> ".html",
   
   "paclet:nrmma/guide/" ~~ ss__ :>
    "/Documentation/English/Guides/" <> StringReplace[ss," "->""] <> ".html"}];

generateHTMLDocumentation[] := Module[
  {exportNotebook, docDir,dest,tutorials,tutorialHTMLNames,tutorialTitles, exportTutorials, exportGuides, exportSymbols},

  exportNotebook[dest_String, nbf_String] :=
  Module[{nb, nn, n2},
         Print["  Exporting ", FileNameTake[nbf,-1]];
         nb = NotebookOpen[nbf];
         nn = NotebookGet[nb]; (* Get expression content from pointer object *)

         (* Fix documentation links since Mathematica repoints them to
            wolfram.com.  Might also be able to use
            ConversionRules. *)
         n2 = nn /. 
         ButtonBox[content_, BaseStyle -> "Link", 
                   ButtonData -> 
                   paclet_?(StringMatchQ[#, 
                                         StartOfString ~~ "paclet:" ~~ __] &)] :> 
         Module[{url = docLink[paclet]},
                ButtonBox[content, BaseStyle -> "Hyperlink",
                          ButtonData -> {URL[url], None}, ButtonNote -> url]];

         Export[dest<>"/" <> 
                StringReplace[FileNameTake[nbf, -1], ".nb" -> ".html"], n2];
         NotebookClose[nb]];

  docDir = FileNameJoin[{FileNameDrop[FindFile["nrmma`"], -2], "Documentation"}];

  dest = If[StringQ[$HTMLDestination], $HTMLDestination, "~/Sites/nrmma/Documentation"];

  exportTutorials[] :=
  Module[
    {tutorials, tutorialHTMLNames, tutorialTitles, tutorialDest},

    tutorials = 
    Select[FileNames["*", FileNameJoin[{docDir, "English/Tutorials"}], 
                     Infinity], ! DirectoryQ[#] &];

    Print["Exporting tutorials"];
    tutorialDest = dest<>"/English/Tutorials";
    Quiet[CreateDirectory[tutorialDest, CreateIntermediateDirectories -> True],CreateDirectory::filex];
    Scan[exportNotebook[tutorialDest,#] &, tutorials];

    tutorialHTMLNames =
    Map[StringReplace[FileNameTake[#, -1], ".nb" -> ".html"] &,
        tutorials];

    tutorialTitles = {
      "BlackHoles.html" -> "Black holes",
      "Binaries.html" -> "Binary systems",
      "DataRegion.html" -> "DataRegion",
      "DataTable.html" -> "DataTable",
      "GridFunctions.html" -> "Grid functions",
      "Kicks.html" -> "Kicks",
      "NRMMAIntroduction.html" -> "Introduction",
      "NumericalRelativity.html" -> "Numerical Relativity"};

    Print["Exporting tutorial index"];

    Export[FileNameJoin[{dest,"English","Tutorials","Tutorials.html"}], 
           Table["<li><a href = \"Documentation/English/Tutorials/"<>g<>"\">"<>(g/.tutorialTitles)<>"</a></li>", {g, tutorialHTMLNames}], "Text"];


    (* Export[dest<>"/list.html", *)
    (*        Map["<li><a href = \"examples/" <> # <> *)
    (*            "\">" <> (# /. tutorialTitles) <> "</a></li>" &, *)
    (*            tutorialHTMLNames], "Text"]; *)

        ];

  exportGuides[] :=
  Module[
    {srcDir, guideNames},
    srcDir = FileNameJoin[{FileNameDrop[FindFile["nrmma`"], -2], "Source"}];
    guideNames = Map[StringReplace[FileNameTake[#,-1],".md"->""] &, sourceGuides];

    Quiet[CreateDirectory[FileNameJoin[{dest,"English","Guides"}],
                          CreateIntermediateDirectories -> True],CreateDirectory::filex];
    Export[FileNameJoin[{dest,"English","Guides","Guides.html"}], 
           Table["<li><a href = \"Documentation/English/Guides/"<>g<>".xml\">"<>g<>"</a></li>", {g, guideNames}], "Text"];

    Scan[(Print[#];
          GenerateHTMLGuidePage[
            #,
            FileNameJoin[{srcDir,"Documentation/English/Guides/"<>#<>".md"}],
            dest]) &, guideNames];
        ];

  exportSymbols[] :=
  Module[
    {srcDir, docSymbols, symbolsSrcDir},

    srcDir = FileNameJoin[{FileNameDrop[FindFile["nrmma`"], -2], "Source"}];
    docSymbols = ToString/@Flatten[Map[Last,packageSymbols],1];
    symbolsSrcDir = FileNameJoin[{srcDir}];

    Print["$Context = ", $Context];
    Print["Global`* contains ", Names["Global`*"]];
    Scan[(Print[#];
          GenerateHTMLSymbolReferencePage[
            #,
            FileNameJoin[{symbolsSrcDir,"Documentation/English/ReferencePages/Symbols/"<>#<>".md"}], 
            dest]) &, docSymbols]];

    exportTutorials[];
    exportGuides[];
    exportSymbols[];
    
    ];

generateHTMLDocumentation[];
        ];

GenerateDocumentation[];

Print["Done"];
