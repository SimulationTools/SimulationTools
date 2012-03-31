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

   {"Title" -> "Data Representations",
    "DetailedFunctions" -> {
      {"DataTable", "representation of 1D data"},
      {"DataRegion", "representation of arbitrary dimensional data on an evenly spaced grid"}
    },
    "Functions" -> {"Frequency", "Phase", "MakeDataRegion", 
      "MergeDataRegions", "ToDataTable", "GetData", "GetOrigin", 
      "MakeDataTable", "IntersectDataTables", "ToList"}
   },

   {"Title" -> "Reading Grid Variables",
    "DetailedFunctions" -> {
      {"ReadGridFunction", "read data for a grid function at a specific iteration"}},
    "Functions" -> {"ReadIterations", "ReadMaps", 
      "ReadRefinementLevels", "ReadTimeLevels", "ReadTime", 
      "ReadVariables"}
   },

   {"Title" -> "Simulation Information",
    "DetailedFunctions" -> {
      {"SimView" , "provide a quick graphical overview of a simulation"},
      {"LookupParameter", "read the value of a specified parameter from a simulation"},
      {"FindParameters", "search for available parameters"}},
    "Functions" -> {"ReadRunSpeed", "ReadWalltime", "ReadCores", 
      "ReadMemory", "ReadSwap", "ReadCPUHours"}
   },

   {"Title" -> "Numerical Relativity",
    "DetailedFunctions" -> {
      {"ReadPsi4" , {"read ", Cell[BoxData[SubscriptBox["\[Psi]", "4"]], "InlineFormula"], "waveform"}},
      {"ReadIsolatedHorizonSpin", "read spin computed by the IsolatedHorizon thorn"},
      {"ReadAHRadius", "read apparent horizon radius from AHFinderDirect thorn"},
      {"ReadBHCoordinate", "read coordinate location of puncture"},
      {"Kick", "compute kick from waveform"}},
    "Functions" -> {"ReadPsi4Modes", "ReadPsi4Radii", "ReadAHMass", 
      "ReadAHSeparation", "ReadAHCentroid", "ReadBHPhase", 
      "ReadBHSeparation", "ReadBHSpeed", "ReadBHTrajectories", 
      "KickVector", "LinearMomentumFlux"}
   },

   {"Title" -> "Visualization",
    "DetailedFunctions" -> {
      {"PresentationArrayPlot", "create a 2D array plot with a legend"},
      {"PresentationListLinePlot", "create a presentation-quality 1D plot"},
      {"DynamicListLinePlot", "version of ListLinePlot which allows zooming by dragging a box with the mouse"},
      {"Grids", "read and plot Carpet grid structures"},
      {"MakeMovie", "generating image files and movies using an external tool (e.g. ffmpeg)"}}
   },

   {"Title" -> "Analysis",
    "DetailedFunctions" -> {
      {"ConvergenceMultiplier", "compute a convergence multiplier given specified resolutions and convergence order"},
      {"ConvergenceRate", "compute the convergence rate as a function of time"}}
   },

   {"Title" -> "Utilities",
    "DetailedFunctions" -> {
      {"ClearAllMemos", "memoisation of functions; i.e. in-memory caching of values so that they don't have to be recomputed"},
      {"FindRunFile", "find instances of a specific file across all restarts"}}
   }

   (* {"Title" -> "Data Sources", *)
   (*  "DetailedFunctions" -> {} *)
   (* } *)

 },
 "Tutorials" -> {
   "NRMMAIntroduction",
   "DataRegion",
   "DataTable",
   "NumericalRelativity",
   "CarpetHDF5",
   "Kicks"
 } 
}