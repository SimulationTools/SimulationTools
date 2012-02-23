
BeginPackage["Stack`"];

WithStackFrame::usage = "WithStackFrame[sf,expr] evaluates expr and adds the stack frame sf to the evaluation stack.";
ShowStack::usage = "ShowStack[] returns a representation of the evaluation stack in a form suitable for display.";
CurrentStack::usage = "CurrentStack[] returns the current evaluation stack.";
ClearStack::usage = "ClearStack[] clears the current evaluation stack.";

Begin["`Private`"];

(* A stack is a list of stack frames.  A stack frame is an expression
   of the form Hold[fn[args...]]. *)

stack = {};

SetAttributes[WithStackFrame, HoldAll];
WithStackFrame[sf_, expr_] := 
  Module[{r},
   AppendTo[stack, sf];
   Catch[r = expr, _, Function[{tag,value}, stack = Drop[stack,-1]; Throw[value,tag]]];
   stack = Drop[stack,-1];
   r];

(* Options[ShowStack] = {"IncludeArguments" -> False}; *)
ShowStack[s_:Automatic] :=
  Module[
    {},
    Print[StringJoin[Riffle[Map["in "<>ToString[#] &, Reverse@If[s===Automatic, stack, s]/.Hold->HoldForm],"\n"]]];
        ];

CurrentStack[] :=
  stack;

ClearStack[] :=
  stack = {};

End[];

EndPackage[];
