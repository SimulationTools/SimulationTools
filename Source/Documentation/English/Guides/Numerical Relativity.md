
{
 "Application" -> "nrmma",
 "Package" -> "Numerical Relativity packages",
 "Title" -> "Numerical Relativity",
 "Summary" -> 
   "Numerical Relativity functionality in NRMMA",
 "Description" -> 
   {"NRMMA provides various functions for analysing Numerical Relativity data "},
 "Keywords" -> {"nrmma", "NRMMA"},
 "Label" -> "Numerical Relativity guide",
 "Synonyms" -> {},
 "URL" -> "http://bitbucket.org/ianhinder/nrmma" ,
 "Packages" -> {

   GuideSection[
     "BHCoordinates",
     {"ReadBHCoordinates" -> "Get the position of one of the black holes in a simulation as a function of time",
     "ReadBHSeparation" -> "Get the separation between the first two black holes in a simulation as a function of time",
     "ReadBHPhase" -> "Get the phase of the relative orbit of the first two black holes in a simulation as a function of time",
     "ReadBHTrajectories" -> "Get the locus of the orbits of the first two black holes in a simulation as a function of time"}],

   GuideSection[
     "Waveforms",
     {"ReadPsi4" -> "Read an (l,m) spherical harmonic mode of Psi4 from a simulation", 
      "ReadPsi4Radii" -> "Get the radii at which Psi4 was decomposed",
      "ReadPsi4Modes" -> "Get the (l,m) modes of Psi4 which are available in a simulation",
      "AlignPhases" -> "Add multiples of 2 Pi to a set of phases such that they are close together at a particular time.  This is necessary due to the 2 Pi ambiguity when converting from real and imaginary to amplitude and phase.",
      "ExtrapolatePsi4" -> "Read an (l,m) mode of Psi4 extrapolated to infinite radius from a simulation",
      "StrainFromPsi4" -> "Convert an (l,m) mode of Psi4 to the corresponding mode of the strain, h.",
      "NumCycles" -> "Read the number of waveform cycles in the (2,2) mode of a simulation"
     }],

   GuideSection[
     "Horizons",
     {"ReadAHCentroid" -> "Read the position of the centroid of an apparent horizon as a function of time from a simulation",
      "ReadAHMass" -> "Read the mass of an apparent horizon as a function of time from a simulation",
      "ReadAHRadius" -> "Read the radius of an apparent horizon as a function of time from a simulation",
     "ReadAHSeparation" -> "Read the distance between the centroids of the first two apparent horizons as a function of time from a simulation",
      "ReadIsolatedHorizonSpin" -> "Read the spin angular momentum of an apparent horizon as a function of time from a simulation"}],

   GuideSection[
     "Kicks",
     {"Kick" -> "Compute the kick in km/s as measured from the gravitational waves",
     "SpinWeightedSphericalHarmonic" -> "Compute a spin-weighted spherical harmonic"}],

   GuideSection[
     "InitialData",{"ReadADMMass" -> "Read the ADM mass of the spacetime from a simulation, computed from the initial data"}],

   GuideSection[
     "CoordinateTransformations",{}],

   GuideSection[
     "DataAnalysis",{"ComputeOverlap" -> "Compute the phase- and time-maximised overlap of two DataTables"}],

   GuideSection[
     "NRExport",{"ExportSim" -> "Export various data from a simulation to a given directory",
                "ExportConfig" -> "Export a set of simulations run at different resolutions"}]},

  "Tutorials" -> {"Numerical Relativity"}}
