
(* A package for dealing with PN circular orbits *)

(* Generally speaking, where available from multiple places, we source
our expressions from the Blanchet Living Review, revised 4-Jul-2006.
We will give references to this review with each expression.  We also
use the notation from this review, and provide a converter to other
notations. *)

BeginPackage["CircularPN`"];

(* PN variables *)

r::usage = "r is the PN coordinate separation of two bodies";
rp0::usage = "rp0 is a PN harmonic coordinate quantity";
om::usage = "om is the relative PN coordinate angular velocity of two bodies";
phi::usage = "phi is the relative PN coordinate phase of two bodies";
psi::usage = "psi is the modified relative PN coordinate phase of two bodies";
m::usage = "m is the total mass of two bodies";
nu::usage = "nu is the symmetric mass ratio of two bodies";
x::usage = "x is a PN coordinate frequency related variable";
En::usage = "En is the orbital energy of two bodies";
eps::usage = "eps is -2 En, where En is the PN orbital energy of two bodies";
mu::usage = "mu is the reduced mass of two bodies";
Madm::usage = "Madm is the PN ADM mass of two bodies";
dot::usage = "dot[f] is the time derivative of f";
h::usage = "h is the gravitational waveform strain";
h22::usage = "h22 is the l = 2, m = 2 mode of the gravitational waveform strain";
psi422::usage = "psi422 is the l = 2, m = 2 mode of Psi4";
psi422Freq::usage = "";
th::usage = "th is the polar spherical coordinate";
ph::usage = "ph is the azimuthal spherical coordinate";
R::usage = "R is the distance to the observer";
om0::usage = "om0 is a constant frequency parameter relating orbital time to retarded time";
gamma;

(* Functions *)

Express::usage = "Express[x,y] expresses quantity x in terms of PN variable y";

SolveOrbitxDotOfx::usage = "SolveOrbitxDotOfx[inits, defs, {t1, t2}]
gives a PN orbit constructed using the TaylorT4 model.  inits and defs
are lists of rules giving various parameters of the orbit.  inits
should contain rules for x and phi, and defs should contain rules for
nu and m.";
RadiusOfOrbit;
EnergyOfOrbit;
EnergyFluxOfOrbit;
FrequencyOfOrbit;

StrainFromOrbit;
Psi4FromStrain;
ComplexFrequency;
ComplexPhase;;

SpinWeightedSphericalHarmonic::usage = "SpinWeightedSphericalHarmonic[s,l,m,th,ph] gives the spin-weighted spherical harmonic";

Begin["`Private`"];

(*--------------------------------------------------------------------
  Orbital coordinates
  --------------------------------------------------------------------*)

(* Blanchet Eq. 194 *)
Express[En,x] = -mu x/2(1+(-3/4-1/12nu)x +(-27/8+19/8nu-nu^2/24)x^2+(-675/64+(34445/576-205/96Pi^2)nu-155/96nu^2-35/5184nu^3)x^3)  +O[x]^5;

Express[x,En] = InverseSeries[Express[En,x], En];

(* Blanchet Eq. 231 *)
Express[L,x] = 32/5 nu^2 x^5(1+(-1247/336-35/12nu) x + 4 Pi x^(3/2)+(-44711/9072+9271/504nu+65/18nu^2)x^2+(-8191/672-583/24nu)Pi x^(5/2)+(6643739519/69854400+16/3Pi^2-1712 EulerGamma/105-856/105Log[16x]+(-134543/7776+41/48Pi^2)nu-94403/3024nu^2-775/324nu^3)x^3+(-16285/504+214745/1728nu+193385/3024nu^2)Pi x^(7/2) + O[x]^4);

Express[dot[En],x] = -Express[L,x];

(* Blanchet Eq. 192 *)
Express[x,om] = (m om)^(2/3);

Express[mu,nu] = m nu;

Express[om,x] = om /. Solve[Express[x,om] == x, om][[1]];

Express[dot[x],x] = Simplify[ComposeSeries[D[Express[x,En], En], Express[En,x]] Express[dot[En],x]];

(* Blanchet Eq. 188 *)
Express[gamma,r] = m/r;

Express[r,gamma] = r /. Solve[Express[gamma,r] == gamma, r][[1]];

(* Blanchet Eq. 193 *)
Express[gamma,x] = x(1 + (1 - nu/3)x + (1 - 65/12nu)x^2 +
  (1 + (-2203/2520-41/192Pi^2-22/3 Log[r/rp0])nu+229/36nu^2+1/81nu^3)x^3)+O[x]^5;

Express[r,x] = Simplify[Express[r,gamma] /. gamma -> Express[gamma,x]];

(* An "orbit" solution consists of interpolating function objects for
om and phi.  How these are obtained is up to the particular solver. *)

SolveOrbitxDotOfx[inits_List, defs_List, {t1_, t2_}, tInit_:0] :=
  Module[{x0, phi0, t, xDot, phiDot, soln},
    Format[t] = Style["t",Bold];
    {x0, phi0} = {x, phi} /. inits;

    xDot = (Normal[Express[dot[x],x]] /. x->x[t] /. mu -> Express[mu,nu] /. defs);
    phiDot = (Normal[Express[om,x]] /. x->x[t] /. mu -> Express[mu,nu] /. defs);

    soln = NDSolve[{D[x[t],t] == xDot, 
                    x[tInit] == x0, 
                    D[phi[t],t] == phiDot, 
                    phi[tInit] == phi0}, {x, phi}, {t, t1, t2}][[1]];
    Return[soln]];

RadiusOfOrbit[soln_, defs_] :=
  Function[t,
    Evaluate[
      Format[t] = Style["t",Bold];
      Normal[Express[r, x] /.(rp0->r) /. x -> x[t] /. mu -> Express[mu,nu]]  /. soln /. defs]];

EnergyOfOrbit[soln_, defs_] :=
  Function[t,
    Evaluate[
      Format[t] = Style["t",Bold];
      Normal[Express[En, x]] /. x -> x[t] /. mu -> Express[mu,nu]  /. soln /. defs]];

EnergyFluxOfOrbit[soln_, defs_] :=
  Function[t,
    Evaluate[
      Format[t] = Style["t",Bold];
      Normal[Express[L, x]] /. x -> x[t] /. mu -> Express[mu,nu]  /. soln /. defs]];

FrequencyOfOrbit[soln_, defs_] :=
  Function[t,
    Evaluate[
      Format[t] = Style["t",Bold];
      Express[om, x] /. x -> x[t] /. mu -> Express[mu,nu]  /. soln /. defs]];

(*--------------------------------------------------------------------
  Waveforms
  --------------------------------------------------------------------*)

(* Blanchet Eq. 226 *)
Express[Madm,gamma] = m (1 - nu/2gamma+nu/8(7-nu)gamma^2+O[gamma]^3);

Express[Madm,x] = ComposeSeries[Express[Madm,gamma], Express[gamma,x]];

Express[Madm,om] = ComposeSeries[Express[Madm,x], Express[x,om] + O[om]^10];

(* Blanchet Eq. 239 *)
Express[psi,{om, Madm}] = phi - 2 Madm om Log[om/om0];

Express[psi,om] = Express[psi,{om, Madm}] /. Madm -> Express[Madm,om];

Express[psi,x] = ComposeSeries[Express[psi, om], Express[om, x] + O[x]^10]

(* Blanchet unnumbered Eq. between 239 and 240 *)
Express[h,x] = 
  Module[{hPlus0, hPlus1, hPlus1p5, hPlus2, hPlus2p5, hCross0, hCross1,hCross1p5, 
          hCross2, hCross2p5, hPlusFullinx, hCrossFullinx},

    hPlus0 = -(1 + Cos[th]^2)Cos[2 (psi - ph)];

    hPlus1 = 1/6(19 + 9Cos[th]^2 - 2Cos[th]^4 - nu(19 - 11Cos[th]^2 - 6Cos[th]^4)) Cos[2 (psi - ph)] - 4/3Sin[th]^2(1 + Cos[th]^2)(1 - 3nu)Cos[4(psi - ph)];

    hPlus1p5 = -2Pi(1 + Cos[th]^2)Cos[2(psi - ph)];

    hPlus2 = 
    1/120(22 + 396Cos[th]^2 + 145Cos[th]^4 - 5Cos[th]^6 + 
            5/3nu(706 - 216Cos[th]^2 - 251Cos[th]^4 + 15Cos[th]^6) - 
            5nu^2(98 - 108Cos[th]^2 + 7Cos[th]^4 + 5Cos[th]^6))Cos[
          2(psi - ph)]
      + 2/
          15Sin[th]^2(59 + 35Cos[th]^2 - 8Cos[th]^4 - 
            5/3nu(131 + 59Cos[th]^2 - 24Cos[th]^4) + 
            5nu^2(21 - 3Cos[th]^2 - 8Cos[th]^4))Cos[4(psi - ph)]
      - 81/40(1 - 5nu + 5nu^2)Sin[th]^4(1 + Cos[th^2]Cos[6(psi - ph)]);

    hPlus2p5 = 
    Pi Cos[2(psi - ph)](19/3 + 3Cos[th]^2 - 2/3Cos[th]^4 + 
            nu(-16/3 + 14/3Cos[th]^2 + 2Cos[th]^4)) + 
      Cos[4(psi - ph)](-16Pi/3(1 + Cos[th]^2)Sin[th]^2(1 - 3nu)) + 
      Sin[2(psi - ph)](-9/5 + 14/5Cos[th]^2 + 7/5Cos[th]^4 + 
            nu(96/5 - 8/5Cos[th]^2 - 28/5Cos[th]^4)) + 
      Sin[th]^2(1 + Cos[th]^2)Sin[
          4(psi - ph)](56/5 - 32Log[2]/3 - nu(1193/30 - 32Log[2]));

    hCross0 = -2Cos[th]Sin[2(psi - ph)];

    hCross1 = 
        Cos[th]/3(17 - 4Cos[th]^2 - nu(13 - 12Cos[th]^2))Sin[2(psi - ph)] - 
          8/3(1 - 3nu)Cos[th]Sin[th]^2Sin[4(psi - ph)];

    hCross1p5 = -4Pi Cos[th]Sin[2(psi - ph)];


    hCross2 = 
    Cos[th]/60(68 + 226Cos[th]^2 - 15Cos[th]^4 + 
            5/3nu(572 - 490Cos[th]^2 + 45Cos[th]^4) - 
            5nu^2(56 - 70Cos[th]^2 + 15Cos[th]^4))Sin[2(psi - ph)] +
      4/15Cos[
          th]Sin[th]^2(55 - 12Cos[th]^2 - 5/3nu(119 - 36Cos[th]^2) + 
            5nu^2(17 - 12Cos[th]^2))Sin[4(psi - ph)]
      - 81/20(1 - 5nu + 5nu^2)Cos[th]Sin[th]^4Sin[6(psi - ph)];

    hCross2p5 = 
        6/5Sin[th]^2Cos[th]nu + 
      Cos[th]Cos[
          2(psi - ph)](2 - 22/5Cos[th]^2 + nu(-154/5 + 94/5Cos[th]^2)) + 
      Cos[th]Sin[th]^2Cos[
          4(psi - ph)](-112/5 + 64/3Log[2] + nu(1193/15 - 64Log[2]))
        +Pi Cos[th]Sin[
          2(psi - ph)](34/3 - 8/3Cos[th]^2 - nu(20/3 - 8Cos[th]^2))
      + Sin[th]^2Cos[th]Sin[4(psi - ph)](-32Pi/3(1 - 3nu));

    hPlusFullinx =  2 mu x / R(hPlus0 + x hPlus1 + x^(3/2) hPlus1p5 + x^2hPlus2 + 
          x^(5/2) hPlus2p5)+O[x]^(8/2);

    hCrossFullinx =  2 mu x / R(hCross0 + x hCross1 + x^(3/2) hCross1p5 + x^2hCross2 + 
          x^(5/2) hCross2p5)+O[x]^(8/2);

    hPlusFullinx - I hCrossFullinx];

SpinWeightedSphericalHarmonic[s_, l_, m_, th_, ph_] =(Factorial[l + m] Factorial[
              l - m] (2l + 1) Factorial[l + s]^(-1) Factorial[
                l - s]^(-1) (4 Pi)^(-1))^(1/2)(Sin[th/2])^(2l) Sum[
        Binomial[l - s, r] Binomial[l + s, r + s - m] (-1)^(l - r - s) Exp[
            I m ph] Cot[th/2]^(2r + s - m), {r, Max[m - s, 0], 
          Min[l - s, l + m]}];

(* y22 = SpinWeightedSphericalHarmonic[-2, 2, 2, th, ph];
   y22Star = ComplexExpand[Conjugate[y22]];
   Express[h22,x] = Integrate[Normal[Express[h,x]] y22Star, 
                              {th, 0, Pi}, {ph, 0, 2 Pi}]; *)

(* We hard-code this for speed - it takes about 3 minutes to compute *)
Express[h22,x] =
  -(mu*Pi^(3/2)*x*(67200 + 4480*(-36 + 13*nu)*x + 134400*Pi*x^(3/2) + 
    3*(-39787 - 83305*nu + 18345*nu^2)*x^2 + 
    448*(-69*I - 720*Pi + 10*nu*(-138*I + 11*Pi))*x^(5/2))*
   (Cos[2*psi] - I*Sin[2*psi]))/(12288*Sqrt[5]*R);

StrainFromOrbit[orbit_List, 2, 2, defs_List] :=
  Function[t, 
    Evaluate[
      Format[t] = Style["t",Bold];
      (* Here we drop the high order psi terms and just use phi *)
      Normal[Express[h22,x]] /. 
        {psi -> (phi[t]/.orbit),
         x -> (x[t]/.orbit), 
         mu -> Express[mu,nu]} /. 
         defs]];

Psi4FromStrain[strainFn_] :=
  Function[t,
    Evaluate[D[strainFn[t],t,t]]];

ComplexFrequency[f_] :=
  Function[t,
    Evaluate[
      Im[D[f[t],t] / f[t]]]];

ComplexPhase[freq_, {t0_, phi0_}, {t1_, t2_}] :=
  p /. NDSolve[{D[p[t],t] == freq[t], p[t0] == phi0}, {p}, {t, t1, t2}][[1]];

End[];

EndPackage[];
