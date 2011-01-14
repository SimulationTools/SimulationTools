(* ::Package:: *)

(* Copyright (C) 2011 Ian Hinder and Barry Wardell *)

BeginPackage["DataAnalysis`", {"DataTable`", "PhysicalConstants`", "Units`"}];

ComputeOverlap::usage = "ComputeOverlap[wf1, wf2, s, f1, f2, M, pad] computes the (maximized over phase and time shift) overlap of the DataTables wf1 and wf2 (assumed to contain the value of  \!\(\*SubscriptBox[\(\[Psi]\), \(4\)]\)) over the frequency range [f1,f2] with the noise function s. The mass of the system is given by M in units of the solar mass. The optional argument pad may be given to pad out the waveform so that a more accurate time shift may be obtained."

Begin["`Private`"];

ComputeOverlap[wf1_, wf2_, s_, f1_, f2_, M_, pad_:None] := 
  Module[{wf1r, wf2r, h1, h2, sn, norm1, norm2, integrand, msun},
    (* Our basic unit it the solar mass in units of seconds *)
    msun = Convert[(SolarMass GravitationalConstant)/SpeedOfLight^3,Second]/Second;

    (* DataTables need to be the same size to compute an overlap. *)
    {wf1r,wf2r} = ResampleDataTables[{wf1,wf2}];

    (* We take the Fourier transform and limit to the requested frequency range. *)
    wf1r = DataTableInterval[Fourier[wf1r], {f1,f2}(M msun)];
    wf2r = DataTableInterval[Fourier[wf2r], {f1,f2}(M msun)];

   (* Convert from Subscript[\[Psi], 4] to h by dividing by (2\[Pi]f)^2 *)
    h1 = MakeDataTable[Transpose[{IndVar[wf1r],DepVar[wf1r]/(2\[Pi] IndVar[wf1r])^2}]];
    h2 = MakeDataTable[Transpose[{IndVar[wf2r],DepVar[wf2r]/(2\[Pi] IndVar[wf2r])^2}]];

    (* Apply units to frequency values *)
    h1 = MakeDataTable[Transpose[{IndVar[h1]/(M msun),DepVar[h1]}]];
    h2 = MakeDataTable[Transpose[{IndVar[h2]/(M msun),DepVar[h2]}]];

    (* Evaluate the noise curve at each point *)
    sn = MapIndVar[s,h1];

    (* Compute normalization *)
    norm1 = Total[Abs[h1]^2/sn];
    norm2 = Total[Abs[h2]^2/sn];

    (* Maximize over time and phase shift using Max and Abs. We use PadRight to enable 
       a more accurate time shift which doesn't need to be an integer multiple of the sampling frequency. *)
    integrand = (h1 Conjugate[h2])/sn;
    If[!SameQ[pad, None], integrand = PadRight[integrand, pad]];

    Max[Abs[InverseFourier[integrand, FourierParameters -> {-1,1}]]]/Sqrt[norm1 norm2]
];

End[];
EndPackage[];
