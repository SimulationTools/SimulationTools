(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["SimulationTools`DataAnalysis`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "PhysicalConstants`",
  "Units`"
 }];

WaveformMatch::usage = "WaveformMatch[{h1, h2}, s] gives the match "<>
 "of the time-domain waveforms h1 and h2 with the noise function s.";
SolarMassInSeconds::usage = "SolarMassInSeconds gives the value of GM/c^3 in seconds, "<>
 "where M is the mass of the sun.";


(****************************************************************)
(* Deprecated *)
(****************************************************************)

ComputeOverlap;

Begin["`Private`"];

(**********************************************************)
(* SolarMassInSeconds                                     *)
(**********************************************************)

SolarMassInSeconds = Convert[(SolarMass GravitationalConstant)/SpeedOfLight^3,Second]/Second;


(**********************************************************)
(* WaveformMatch                                          *)
(**********************************************************)

SyntaxInformation[WaveformMatch] =
 {"ArgumentsPattern" -> {{_, _}, _, ___}};

Options[WaveformMatch] = {"Pad" -> None};

DocumentationBuilder`MoreInformation["WaveformMatch"] =
 {
  "The match is maximized over phase and time shifts.",
  "The noise function s must be a DataTable defined on the frequency range to be integrated over.",
  "The match is computed as given by Eq. (4) of Phys. Rev. D 84, 064029 (2011).",
  "The units of the waveforms and of the noise function must be consistent. For noise "<>
  "curves given in Hz, this means the independent variable in the waveforms must be in "<>
  "units of seconds."
 };

DocumentationBuilder`OptionDescriptions["WaveformMatch"] =
 {
  "Pad" ->  "The option Pad may be given to pad out the waveform to the specified size "<>
            "so that a more accurate time shift can be obtained."
 };

WaveformMatch[{wf1_DataTable, wf2_DataTable}, s_DataTable, OptionsPattern[]] :=
  Module[{wf1t, wf2t, sn, norm1, norm2, integrand, pad},
    (* We take the Fourier transform of the waveforms *)
    wf1t = Fourier[wf1];
    wf2t = Fourier[wf2];

	(* Resample the waveforms and noise function onto the same grid *)
    {wf1t, wf2t, sn} = SimulationTools`DataTable`Private`resampled[{wf1t, wf2t, s}];

    (* Compute normalization - First here indicates no time shift *)
    norm1 = 4 First[Abs[InverseFourier[Abs[wf1t]^2/sn, FourierParameters -> {-1,1}]]];
    norm2 = 4 First[Abs[InverseFourier[Abs[wf2t]^2/sn, FourierParameters -> {-1,1}]]];

    (* Compute the inner product integrand *)
    integrand = (wf1t Conjugate[wf2t]) / sn;

    (* Use PadRight to enable a more accurate time shift which doesn't need to be an
       integer multiple of the sampling frequency. *)
    pad = OptionValue["Pad"];
    If[!SameQ[pad, None], integrand = PadRight[integrand, pad]];

    (* Compute the match integral maximized over time and phase shift using Max and Abs. *)
    4 Max[Abs[InverseFourier[integrand, FourierParameters -> {-1,1}]]]/Sqrt[norm1 norm2]
];



(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

ComputeOverlap[wf1_, wf2_, s_, f1_, f2_, M_, pad_:None] := 
  Module[{wf1r, wf2r, h1, h2, sn, norm1, norm2, integrand, msun},
    (* Our basic unit it the solar mass in units of seconds *)
    msun = SolarMassInSeconds;

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
