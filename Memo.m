
BeginPackage["Memo`"];

Memo;
DefineMemoFunction;
ClearAllMemos;

Begin["`Private`"];

SetAttributes[DefineMemoFunction, HoldAll];

Memo[f_] :=
   Function[
       If[Head[DataCache[f, {##}]] === DataCache,
          DataCache[f, {##}] = Apply[f,{##}]];
       DataCache[f, {##}]];

DefineMemoFunction[name_[args___], body_] :=
  Module[{f},
    SetDelayed[f[args], body];
    name = Memo[f]];

ClearAllMemos[] :=
  Clear[DataCache];

End[];

EndPackage[];
