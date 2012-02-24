
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
  Module[{r,oldlen, head},
   AppendTo[stack, Hold[sf]];
   head = Hold[sf][[1,0]];
   oldlen = Length[stack];
   CheckAbort[Catch[r = expr, _, 
                    Function[{value,tag},
                             Print["Dropping stack frame due to exception"];
                             stack = Drop[stack,-1];
                             Throw[value,tag]]],
              stack = Drop[stack,-1];Abort[]];
   If[Length[stack] =!= oldlen,
      Print["Error: Stack length changed during ", head,
            "; current top of stack is ", Short[stack[[-1]]/.Hold->HoldForm]]];
   stack = Drop[stack,-1];
   r];

(* Options[ShowStack] = {"IncludeArguments" -> False}; *)
ShowStack[s_:Automatic] :=
  Scan[
    Print["in ", Short[#]] &,
    Reverse@If[s===Automatic, stack, s]/.Hold->HoldForm];

CurrentStack[] :=
  stack;

ClearStack[] :=
  stack = {};

End[];

EndPackage[];
