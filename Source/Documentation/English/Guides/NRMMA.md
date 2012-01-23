{
 "Application" -> "nrmma",
 "Package" -> "nrmma",
 "Title" -> "NRMMA Package",
 "Summary" -> 
   "A package for analysing data in Numerical Relativity.",
 "Description" -> 
   {"NRMMA is a suite of ", StyleBox["Mathematica", FontSlant->"Italic"], " packages for analysing data in Numerical Relativity.  It has been designed for use with common output formats used by the Cactus code, with a focus on output from the PSU and AEI codes."},
 "Keywords" -> {"nrmma", "NRMMA"},
 "Label" -> "NRMMA Application",
 "Synonyms" -> {"NRMMA", "nrmma"},
 "URL" -> "http://bitbucket.org/ianhinder/nrmma" ,
 "Packages" -> {
   {"Title" -> "Numerical Relativity",
    "DetailedFunctions" -> {
      {"ReadPsi4" , {"read ", Cell[BoxData[SubscriptBox["\[Psi]", "4"]], "InlineFormula"], "waveform"}},
      {"ReadIsolatedHorizonSpin", "read spin computed by the IsolatedHorizon thorn"},
      {"ReadAHRadius", "read apparent horizon radius from AHFinderDirect thorn"},
      {"ReadBHCoordinate", "read coordinate location of puncture"},
      {"Kick", "compute kick from waveform"},
      {"QuasinormalMode", "compute quasinormal mode"}},
    "Functions" -> {"ReadPsi4Modes", "ReadPsi4Radii", "ReadAHMass", 
      "ReadAHSeparation", "ReadAHCentroid", "ReadBHPhase", 
      "ReadBHSeparation", "ReadBHSpeed", "ReadBHTrajectories", 
      "KickVector", "LinearMomentumFlux"}
   },
   {"Title" -> "Simulation Monitoring",
    "DetailedFunctions" -> {
      {"SimView" , "provide a quick graphical overview of a simulation"},
      {"LookupParameter", "read the value of a specified parameter from a simulation"},
      {"FindParameters", "search for available parameters"}},
    "Functions" -> {"ReadRunSpeed", "ReadWalltime", "ReadCores", 
      "ReadMemory", "ReadSwap", "ReadCPUHours"}
   },
   {"Title" -> "Reading Data",
    "DetailedFunctions" -> {
      {"ReadGridFunction", "read data for a grid function at a specific iteration"},
      {"ReadCarpetASCII1D", "read 1D ASCII data from a file"},
      {"FindRunFile", "find instances of a specific file across all restarts"},
      {"ReadTimerFromRun", "read TimerReport output files"}(* ,
      {"Grids", "read and plot Carpet grid structures"} *)},
    "Functions" -> {"ReadIterations", "ReadMaps", 
      "ReadRefinementLevels", "ReadTimeLevels", "ReadTime", 
      "ReadVariables"}
   },
   {"Title" -> "Data Manipulation",
    "DetailedFunctions" -> {
      {"DataTable", "representation of 1D data"},
      {"DataRegion", "representation of arbitrary dimensional data on an evenly spaced grid"}
    },
    "Functions" -> {"Frequency", "Phase", "MakeDataRegion", 
      "MergeDataRegions", "ToDataTable", "GetData", "GetOrigin", 
      "MakeDataTable", "IntersectDataTables", "ToList"}
   },
   {"Title" -> "Data Analysis",
    "DetailedFunctions" -> {
      {"ConvergenceMultiplier", "compute a convergence multiplier given specified resolutions and convergence order"},
      {"ConvergenceRate", "compute the convergence rate as a function of time"}}
   },
   {"Title" -> "Data Visualization",
    "DetailedFunctions" -> {
      {"PresentationArrayPlot", "create a 2D array plot with a legend"},
      {"PresentationListLinePlot", "create a presentation-quality 1D plot"},
      {"DynamicListLinePlot", "version of ListLinePlot which allows zooming by dragging a box with the mouse"},
      {"MakeMovie", "generating image files and movies using an external tool (e.g. ffmpeg)"}}
   },
   {"Title" -> "Utilities",
    "DetailedFunctions" -> {
      {"Memo", "memoisation of functions; i.e. in-memory caching of values so that they don't have to be recomputed"},
      {"Profile", "add timing information to functions which can be queried later"}}
   }
 },
 "Tutorials" -> {
   "CarpetHDF5",
   "DataRegion",
   "DataTable",
   "Kicks",
   "NumericalRelativity"
 } 
}