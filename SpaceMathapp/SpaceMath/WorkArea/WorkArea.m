(* Wolfram Language Package *)

WinRXX::usage="WinRXX"

Begin["`Package`"]
End[]

Begin["`WorkArea`Private`"]

WinRXX[]:= CreateWindow[
  					DialogNotebook[
  						{
  						  TextCell["Enter a name: "], 
    					  InputField[Dynamic[nm], String], 
    				      DefaultButton[DialogReturn[ret = nm]]
    				     }
    				    ]
    				   ];

End[]
