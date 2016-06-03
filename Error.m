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

BeginPackage["SimulationTools`Error`", {"StackTrace`"}];

Error(*::usage = "Error[message, arg1, arg2, ...] reports an error to the user."*);
WithExceptions;
WithExceptionsFn;
Error::error = "An error has been detected: `1`";

(* TODO: this should not be set on package load as it will override the user's choice *)
$MessageOnError = False;


Begin["`Private`"];

$ExceptionsActive = {};

(****************************************************************)
(* Error                                                        *)
(****************************************************************)

Error[s_String, args___] :=
  Module[
    {},
	(*  We probably don't need both the message and the assertion; either is
        sufficient to trigger the debugger.  Also, we should support this
        mechanism for the symbol version of this function as well. *)
    If[$MessageOnError,Message[Error::error, ToString[StringForm[s,args]]]];
    Print[StringForm[s,args]];
    Print["in ", Sequence@@Riffle[CurrentStackTrace[],"/\n   "]];
    Assert[False]; (* Allow this to be caught by the debugger *)
    Abort[]];

Error[e_Symbol, s_String, args___] :=
  If[!MemberQ[$ExceptionsActive, e],
     Error[StringReplace[s <> " ("<>ToString[e]<>")","`"->"`.`"],args],
     Throw[If[{args}==={}, s, {s,args,CurrentStackTrace[]}], e]];

(****************************************************************)
(* CatchException                                               *)
(****************************************************************)

SetAttributes[WithExceptions, HoldAll];
WithExceptions[expr_, exception_ -> result_] :=
  (* Only support a single exception at a time for the moment *)
  Module[
    {expr1},
    expr1 := expr; (* Ensure that if Return is called in expr, we return to the correct place *)
    Catch[Block[{$ExceptionsActive = Append[$ExceptionsActive, exception]}, expr1],
          exception,
          result &]];

SetAttributes[WithExceptionsFn, HoldAll];
WithExceptionsFn[expr_, exception_ -> resultFn_] :=
  (* Only support a single exception at a time for the moment *)
  Module[
    {expr1},
    expr1 := expr; (* Ensure that if Return is called in expr, we return to the correct place *)
    Catch[Block[{$ExceptionsActive = Append[$ExceptionsActive, exception]}, expr1],
          exception,
          resultFn]];

WithExceptions[___] :=
  Error["Invalid arguments to WithExceptions"];

End[];

EndPackage[];
