(* Wolfram Language Package *)

Win::usage="WinX"

Begin["`Package`"]
End[]

Begin["`WorkArea`Private`"]

WinX[]:= CreateWindow[
  DialogNotebook[{TextCell["Enter a name: "], 
    InputField[Dynamic[nm], String], 
    DefaultButton[DialogReturn[ret = nm]]}]];

End[]
