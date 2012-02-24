
BeginPackage["Stack`"];

WithStackFrame::usage = "WithStackFrame[sf,expr] evaluates expr and adds the stack frame sf to the evaluation stack.";
ShowStack::usage = "ShowStack[] returns a representation of the evaluation stack in a form suitable for display.";
CurrentStack::usage = "CurrentStack[] returns the current evaluation stack.";
ClearStack::usage = "ClearStack[] clears the current evaluation stack.";
CurrentStackFrame;

Begin["`Private`"];

(* A stack is a list of stack frames.  A stack frame is an expression
   of the form Hold[fn[args...]]. *)

stack = {};

SetAttributes[WithStackFrame, HoldAll];
WithStackFrame[sf_, expr_] := 
  Module[{r,oldlen, head, oldStack, locFn},
   oldStack = stack;
   stack = Append[oldStack, Hold[sf]];
   head = Hold[sf][[1,0]];

   locFn := expr; (* This is necessary in case expr contains a Return
                     statement.  Return exits the nearest-enclosing
                     "control structure", which unless we use this
                     construct, might be WithStackFrame! *)

   CheckAbort[Catch[r = locFn, _, 
                    Function[{value,tag},
                             (* Print["Resetting stack due to exception during ",Short[Hold[sf]/.Hold->HoldForm]]; *)
                             (* Print["Exception occured in ", Short[stack[[-1]]/.Hold->HoldForm]]; *)

                             If[Length[stack] =!= Length[oldStack]+1,
                                Print["Error: Stack length changed during ", head,
                                      "; current top of stack is ", Short[stack[[-1]]/.Hold->HoldForm]]];


                             stack = oldStack;
                             Throw[value,tag]]],
              (* Print["Resetting stack due to Abort"]; *)
              stack = oldStack;
              Abort[]];

   If[Length[stack] =!= Length[oldStack]+1,
      Print["Error: Stack length changed during ", Short[Hold[sf]/.Hold->HoldForm],
            "; current top of stack is ", Short[stack[[-1]]/.Hold->HoldForm]]];
   
   stack = oldStack;
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

CurrentStackFrame[] :=
  Short[stack[[-1]]/.Hold->HoldForm];

End[];

EndPackage[];
