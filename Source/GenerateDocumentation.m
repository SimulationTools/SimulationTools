<< SimulationTools`;
<< ApplicationTools`;

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

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
  "SimulationProperties",
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

packageSymbols = Map[# -> DocumentedSymbols["SimulationTools", #] &, packages];

appSymbols = DocumentedSymbols["SimulationTools"];

$PackageSymbols = packageSymbols; (* Used in the Overview.md file *)

undocumentedSymbols = Map[# -> UndocumentedSymbols["SimulationTools", #] &, packages] /. (_ -> {}) -> Sequence[];
(* Map[Print["Undocumented symbols for package "<>#[[1]]<>" skipped:\n", #[[2]]]&, undocumentedSymbols]; *)

Print["Building symbol reference pages"];
docPackage[package_ -> symbols_] :=
  Map[(Print[#]; BuildSymbolReference["SimulationTools", #, "Source"]) &, symbols];
Scan[docPackage, packageSymbols];
docPackage["SimulationTools" -> appSymbols];

Print["Building guides"];
sourceGuides = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Guides"}], Infinity];
destGuides =
  FileNameJoin[{Directory[], FileNameDrop[DirectoryName[#], 1],
      FileBaseName[#] <> ".nb"}] & /@ sourceGuides;
MapThread[BuildGuide, {sourceGuides, destGuides}];

Print["Building tutorials"];
tutorialSources = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Tutorials"}], Infinity];
Map[(Print[#]; BuildTutorial[FileNameJoin[{Directory[], #}]])&, tutorialSources];

Print["Changing FrontEnd version to 8.0"];
fixupVersion[nbfile_] :=
  Export[nbfile,
    StringReplace[Import[nbfile, "String"],
      "FrontEndVersion->\"" ~~ __ ~~ "\\\n" ~~ DigitCharacter .. ~~ ")\"," ->
      "FrontEndVersion->\"8.0 for Mac OS X x86 (32-bit, 64-bit Kernel) (October 5, \\\n2011)\","],
    "String"
  ];
notebooks = FileNames["*.nb", FileNameJoin[{"Documentation"}], Infinity];
fixupVersion /@ notebooks;

Print["Indexing Documentation"];
BuildIndex["SimulationTools"];

(**********************************************************************************************************)
(* HTML Documentation *)
(**********************************************************************************************************)

Print["Exporting HTML Documentation"];

(* Return the path of the Documentation directory in the passed filename *)
docRoot[nb_String] :=
  StringReplace[nb, "/Documentation/*" -> "/Documentation"];

docLink[s_String, from_String] :=
Module[{doc = docRoot[from]},
 subPath = StringReplace[from, __~~"/Documentation/English/" -> ""];

 s2 = StringReplace[s,
  {"paclet:SimulationTools/ref/" ~~ ss__ :> 
    "ReferencePages/Symbols/" <> ss <> 
     ".xml",
   
   "paclet:SimulationTools/tutorial/" ~~ ss__ :> 
    "Tutorials/" <> StringReplace[ss," "->""] <> ".html",
   
   "paclet:SimulationTools/guide/" ~~ ss__ :>
    "Guides/" <> StringReplace[ss," "->""] <> ".xml"}];

  (* s2 is the link target relative to Documentation/English.  subPath
     is the link host notebook path relative to Documentation/English. *)

  StringJoin[ConstantArray["../", FileNameDepth[subPath]]]<>s2];

skipTargetQ[src_, dest_] :=
    If[FileType[dest] =!= None,
       Module[
         {diff = DateDifference[FileDate[src], FileDate[dest]]},
         (* Print["src = ", src]; *)
         (* Print["dest = ", dest]; *)
         (* Print["diff = ", diff]; *)
         Return[diff > 0]],
       False];

generateHTMLDocumentation[] := Module[
  {exportNotebook, docDir,dest, exportTutorials, exportGuides, exportSymbols},

  exportNotebook[dest_String, nbf_String] :=
  Module[{nb, nn, n2, destName},
         destName = dest<>"/" <> 
                StringReplace[FileNameTake[nbf, -1], ".nb" -> ".html"];

         If[skipTargetQ[nbf, destName],
            Print["Skipping ", FileNameTake[nbf,-1]];
            Return[]];

         (* If[FileType[destName] =!= None, *)
         (*    Module[ *)
         (*      {diff = DateDifference[FileDate[nbf], FileDate[destName]]}, *)
         (*      Print[diff]; *)
         (*      If[diff > 0, *)
         (*         Print["Skipping ", FileNameTake[nbf,-1]]; *)
         (*         Return[]]]]; *)

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
         Module[{url = docLink[paclet, dest]},
                ButtonBox[content, BaseStyle -> "Hyperlink",
                          ButtonData -> {URL[url], None}, ButtonNote -> url]];

         Export[destName, n2];
         NotebookClose[nb]];

  docDir = FileNameJoin[{$SimulationToolsInstallationDirectory, "Documentation"}];

  dest = If[StringQ[$HTMLDestination],
            $HTMLDestination,
            FileNameJoin[{$SimulationToolsInstallationDirectory, "Source","HTMLDoc/Documentation"}]];

  Quiet[CreateDirectory[dest, CreateIntermediateDirectories -> True],CreateDirectory::filex];

  exportTutorials[] :=
  Module[
    {tutorials, tutorialHTMLNames, tutorialTitles, tutorialDest, srcDir},

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

    (* Fixup tutorial CSS *)
    Map[Export[#, StringReplace[Import[#, "String"], 
        "\"HTMLFiles/" ~~ LetterCharacter .. ~~ ".css\"" -> "\"tutorial.css\""], "String"] &, 
      FileNameJoin[{tutorialDest, #}] & /@ tutorialHTMLNames];
    srcDir = FileNameJoin[{$SimulationToolsInstallationDirectory, "Source"}];
    DeleteFile[FileNameJoin[{tutorialDest, "tutorial.css"}]];
    CopyFile[FileNameJoin[{srcDir, "tutorial.css"}], FileNameJoin[{tutorialDest, "tutorial.css"}]];

    tutorialTitles = {
      "BlackHoles.html" -> "Black holes",
      "Binaries.html" -> "Binary systems",
      "DataRegion.html" -> "DataRegion",
      "DataTable.html" -> "DataTable",
      "GridFunctions.html" -> "Grid functions",
      "Kicks.html" -> "Kicks",
      "SimulationToolsIntroduction.html" -> "Introduction",
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
    srcDir = FileNameJoin[{$SimulationToolsInstallationDirectory, "Source"}];
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

    srcDir = FileNameJoin[{$SimulationToolsInstallationDirectory, "Source"}];
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
