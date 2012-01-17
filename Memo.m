(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Memo`"];

Memo::usage = "Memo is a symbol used by the Memo package.  It is not needed by end-users.";
DefineMemoFunction::usage = "DefineMemoFunction[f[args], body] defines a function f with arguments args and body body such that the result of the function is remembered after the first time it is called.  This is used to cache data that is expensive to read or compute.  See ClearAllMemos for flushing this cache.";
ClearAllMemos::usage = "ClearAllMemos[] wipes the cache of functions that have been memoised using DefineMemoFunction.";

Begin["`Private`"];

SetAttributes[DefineMemoFunction, HoldAll];

Memo[f_] :=
   Function[
       If[Head[DataCache[f, {##}]] === DataCache,
          DataCache[f, {##}] = Apply[f,{##}]];
       DataCache[f, {##}]];

DefineMemoFunction[name_[args___], body_] :=
  Module[{f},
    Options[f] = Options[name];
    SetDelayed[f[args], body];
    name = Memo[f]];

ClearAllMemos[] :=
  Clear[DataCache];

End[];

EndPackage[];
