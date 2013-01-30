
{
 "Application" -> "SimulationTools",
 "Package" -> "Numerical Relativity packages",
 "Title" -> "Numerical Relativity",
 "Summary" -> 
   "Numerical Relativity functionality in SimulationTools",
 "Description" -> 
   {"SimulationTools provides various functions for analysing Numerical Relativity data "},
 "Keywords" -> {"SimulationTools", "SimulationTools"},
 "Label" -> "Numerical Relativity guide",
 "Synonyms" -> {},
 "URL" -> "http://bitbucket.org/ianhinder/SimulationTools" ,
 "Packages" -> {

   GuideSection[
     "BHCoordinates",
     {"ReadBinaryCoordinates" -> "Get the coordinates of a binary system as a function of time",
     "ReadBinarySeparation" -> "Get the separation between the members of a binary system as a function of time",
     "ReadBinaryPhase" -> "Get the phase of the relative orbit of a binary system as a function of time"}],

   GuideSection[
     "Waveforms",
     {"ReadPsi4" -> "Read an (l,m) spherical harmonic mode of Psi4 from a simulation", 
      "ReadPsi4Radii" -> "Get the radii at which Psi4 was decomposed",
      "ReadPsi4Modes" -> "Get the (l,m) modes of Psi4 which are available in a simulation",
      "AlignedPhases" -> "Add multiples of 2 Pi to a set of phases such that they are close together at a particular time.  This is necessary due to the 2 Pi ambiguity when converting from real and imaginary to amplitude and phase.",
      "ReadRadiallyExtrapolatedPsi4" -> "Read an (l,m) mode of Psi4 extrapolated to infinite radius from a simulation",
      "Psi4ToStrain" -> "Convert an (l,m) mode of Psi4 to the corresponding mode of the strain, h.",
      "ReadWaveformCycles" -> "Read the number of waveform cycles in the (2,2) mode of a simulation."
     }],

   GuideSection[
     "Horizons",
     {(* "ReadAHCentroid" -> "Read the position of the centroid of an apparent horizon as a function of time from a simulation", *)
      "ReadBlackHoleMass" -> "Read the mass of a black hole as a function of time from a simulation",
      (* "ReadAHRadius" -> "Read the radius of an apparent horizon as a function of time from a simulation", *)
     (* "ReadAHSeparation" -> "Read the distance between the centroids of the first two apparent horizons as a function of time from a simulation", *)
      "ReadBlackHoleSpin" -> "Read the spin angular momentum of a black hole as a function of time from a simulation"}],

   (* GuideSection[ *)
   (*   "Kicks", *)
   (*   {"ReadKick" -> "Compute the kick in km/s as measured from gravitational waves"}], *)

   GuideSection[
     "InitialData",{"ReadADMMass" -> "Read the ADM mass of the spacetime from a simulation, computed from the initial data"}],

   GuideSection[
     "CoordinateTransformations",{}],

   (* GuideSection[ *)
   (*   "DataAnalysis",{"WaveformMatch" -> "Compute the phase- and time-maximised overlap of two DataTables"}], *)

   GuideSection[
     "NRExport",{"ExportNumericalRelativitySimulation" -> "Export various data from a simulation to a given directory"}]},

  "Tutorials" -> {"Numerical Relativity"}}
