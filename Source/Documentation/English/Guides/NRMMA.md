{
 "Application" -> "nrmma",
 "Package" -> "nrmma",
 "Title" -> "NRMMA Package",
 "Summary" -> 
   "A package for analysing data in Numerical Relativity.",
 "Description" -> 
   {"NRMMA is a ", StyleBox["Mathematica", FontSlant->"Italic"],
    " application for analysing data from numerical simulations.  See ",
    TutorialLink["nrmma", "NRMMAIntroduction"], " for an introduction."},
 "Keywords" -> {"nrmma", "NRMMA"},
 "Label" -> "NRMMA Application",
 "Synonyms" -> {"NRMMA", "nrmma"},
 "URL" -> "http://bitbucket.org/ianhinder/nrmma" ,
 "Packages" -> {

   {"Title" -> "Data Representations",
    "Link" -> "paclet:nrmma/guide/Data Representations",
    "DetailedFunctions" -> {
      {"DataTable"},
      {"DataRegion"}
    },
    "Functions" -> {"Frequency", "Phase", "ToDataRegion", 
      "ToDataTable", "ToListOfData", "MinCoordinates", 
      "RestrictedToCommonInterval", "ToList"},
    "MoreFunctionsLink" -> "paclet:nrmma/guide/Data Representations"
   },

   {"Title" -> "Reading Grid Variables",
    "DetailedFunctions" -> {
      {"ReadGridFunction"}},
    "Functions" -> {"ReadIterations", "ReadMaps", 
      "ReadRefinementLevels", "ReadTimeLevels"}
   },

   {"Title" -> "Simulation Information",
    "DetailedFunctions" -> {
      {"SimulationOverview"},
      {"ReadSimulationParameter"},
      {"FindSimulationParameters"}},
    "Functions" -> {"ReadSimulationSpeed", "ReadSimulationRunTime", "ReadSimulationCoreCount", 
      "ReadSimulationMemoryUsage", "ReadSimulationSwapUsage", "ReadSimulationCost"}
   },

   {"Title" -> "Numerical Relativity",
    "Link" -> "paclet:nrmma/guide/Numerical Relativity",
    "DetailedFunctions" -> {
      {"ReadPsi4"},
      {"ReadBlackHoleSpin"},
      {"ReadBinaryCoordinates"},
      {"ReadKick"}},
    "Functions" -> {"ReadPsi4Modes", "ReadPsi4Radii", "ReadBlackHoleMass", 
      "ReadBinarySeparation", "ReadBinaryPhase", 
      "ReadKickVector", "ReadLinearMomentumFlux"},
    "MoreFunctionsLink" -> "paclet:nrmma/guide/Numerical Relativity"
   },

   (* {"Title" -> "Visualization", *)
   (*  "DetailedFunctions" -> { *)
   (*    {"PresentationArrayPlot", "create a 2D array plot with a legend"}, *)
   (*    {"PresentationListLinePlot", "create a presentation-quality 1D plot"}, *)
   (*    {"DynamicListLinePlot", "version of ListLinePlot which allows zooming by dragging a box with the mouse"}, *)
   (*    (\* {"Grids", "read and plot Carpet grid structures"}, *\) *)
   (*    {"MakeMovie", "generating image files and movies using an external tool (e.g. ffmpeg)"}} *)
   (* }, *)

   {"Title" -> "Analysis",
    "DetailedFunctions" -> {
      {"ConvergenceMultiplier"},
      {"ConvergenceRate"}}
   },

   {"Title" -> "Utilities",
    "DetailedFunctions" -> {
      {"ClearAllMemos"},
      {"FindSimulationFiles"}}
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
   "GridFunctions",
   "Kicks"
 } 
}