(* Wolfram Language Package *)

WinArea::usage="WinArea"

WinTestXX::usage="WinTestXX"

WinTest::usage="WinTest"

WinSMA1::usage="WinSMA1"



Begin["`Package`"]
End[]

Begin["`WorkArea`Private`"]

WinTest[]:= CreateWindow[
  					DialogNotebook[
  						{
  						  TextCell["Enter a name: "], 
    					  InputField[Dynamic[nm], String], 
    				      DefaultButton[DialogReturn[ret = nm]]
    				     }
    				    ]
    				   ];
 
 WinArea[]:=
 Panel[
DynamicModule[
	      {
	       ghttcode = Hold[ghtt[a_,Att_,Cab_,tb_] :=(g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att)))], 
  		 ghbbcode = Hold[ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)))],
		   ghtautaucode = Hold[ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)))],
		   ghWWcode = Hold[ghWW[sab1_] := gw*mW*sab1],
		   ghZZcode = Hold[ghZZ[sab1_] := gz*mZ*sab1],
	       a1 = ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
	       Att = 1,
	       Cab = Sqrt[1 - sab^2],
	       tb = 1,
	       sab1 = sab, 
	       P1Min = -1, 
	       P1Max = 1,
	       SMsigma = 1,
	       LabelXX1 = LabelXX,
	       P1step=0.001
	      },
  	      With[
  	           {
  	            fn1 = (ghttcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn2 = (ghbbcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn3 = (ghtautaucode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn4 = (ghWWcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn5 = (ghZZcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &
  	           }, 
   	       Manipulate[
   	         Dynamic[                                             
                  Switch[
                         tabNumber,
                         tab1,
                             Grid[
                                 {
                                  {Hyperlink[Button[Style["Documentation Center", FontSize -> 14, FontColor -> Black, FontWeight -> Normal], Background -> Lighter[Brown, 0.5], ImageSize -> {140, 50}, Method -> "Queued"], "paclet:SpaceMath/tutorial/SpaceMathOverview"],
                                   DisplayForm@ButtonBox[Style["Examples",FontSize -> 14], ButtonFunction :> SystemOpen[FileNameJoin[{$SpaceMathDirectory, "Examples"}]], Evaluator -> Automatic, Method -> "Preemptive", Appearance -> "DialogBox", Background -> Lighter[Brown, 0.5], ImageSize -> {100, 35}],
                                   Button[Style["Cite", FontSize -> 14], CreateWindow[DialogNotebook[{Button[Style["BibTex"], 
                                     CreateWindow[DialogNotebook[{CopyText1 = TextCell["@article{Arroyo-Urena:2020qup, 
                                     author = \"Arroyo-Ure\~{n}a, M.A. and Gait\'an, R. and Valencia-P\'erez, T.A.\",
                                     title = \"{$\ \\texttt{SpaceMath}$ version 1.0. $\\\\$ A $\\texttt{Mathematica}$ package for beyond the standard model parameter space searches}\",
                                     eprint = \ \"2008.00564\", archivePrefix\ = \"arXiv\", primaryClass \ = \"hep-ph\",
                                     month = \"8\", year = \ \"2020\"}"], 
                                     Button["Copy to clipboard", CopyToClipboard[CopyText1]], Button["Close", DialogReturn[]]}]], Background -> Lighter[Brown, 0.5], ImageSize -> {60, 25}], 
                                     Button[Style["Bibitem"], CreateWindow[DialogNotebook[{CopyText2 = TextCell["%\\cite{Arroyo-Urena:2020qup}
                                     \\bibitem{Arroyo-\ Urena:2020qup} M. A. Arroyo-\ Ure\~{n}a, R. Gait\'an and T. A. Valencia-P\'erez,
                                     %``$\\texttt{\ SpaceMath}$ version 1.0. $\\\\$ A $\\texttt{Mathematica}$ package for beyond the standard model parameter space searches,'' [arXiv:2008.\ 00564 [hep-ph]]."], 
                                     Button["Copy to clipboard", CopyToClipboard[CopyText2]], Button["Close", DialogReturn[]]}]], Background -> Lighter[Brown, 0.5], ImageSize -> {60, 25}],
                                     Button["Close", DialogReturn[]]}]], Background -> Lighter[Brown, 0.5], ImageSize -> {60, 25}]
                                  ,4},
                                  {"SpaceMath version 1.0. A Mathematica package for beyond the standard model parameter space searches",
                                  SpanFromLeft,SpanFromLeft,SpanFromLeft},
                                  {Style["Authors:\n  \[CirclePlus] M. A. Arroyo-Ureña\n Facultad de Estudios Superiores-Cuautitlán, Universidad Nacional Autónoma de México\n \
                                          \[CircleTimes] T. A. Valencia-Pérez\n Instituto de Física, Universidad Nacional Autónoma de México\n Contact us:	\
                                          spacemathapp@gmail.com", "Text", FontSize -> 16, FontColor -> Black],
                                   SpanFromLeft,SpanFromLeft,SpanFromLeft}
                                  }, Frame -> All,Alignment -> Top],
                         tab2,
                               Grid[
                                 {
                                  {"Tutorial",2,3,
                                  Hyperlink[Button[Style["Documentation Center", FontSize -> 14, FontColor -> Black, FontWeight -> Normal], Background -> Lighter[Brown, 0.5], ImageSize -> {140, 50}, Method -> "Queued"], "paclet:SpaceMath/tutorial/SpaceMathOverview"]
                                  },
                                  {1,"Tutorial",3,
                                  DisplayForm@ButtonBox[Style["Examples",FontSize -> 14], ButtonFunction :> SystemOpen[FileNameJoin[{$SpaceMathDirectory, "Examples"}]], Evaluator -> Automatic, Method -> "Preemptive", Appearance -> "DialogBox", Background -> Lighter[Brown, 0.5], ImageSize -> {100, 35}]
                                  },
                                  {1,2,"Tutorial",4}
                                 }, Frame -> All,Alignment -> Top],
                         tab3,
                            Grid[
                                 {
                                  {Style["HIGGS BOSON DATA","Subtitle"],SpanFromLeft},
                                  {Style["Signal Strengths \!\(\*FormBox[SubscriptBox[\(R\), \(X\)], TraditionalForm]\)",Bold,Large], SpanFromAbove},
                                  {Style["Enter couplings",Bold,Medium], SpanFromAbove},
                                  {Style["ghtt[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghttcode, fn1], Hold[Expression]],"ghtt", Left], FigRZone = Dynamic[ RZone[ghtt[a1,Att,Cab,tb], ghbb[a1,Att,Cab,tb], ghZZ[sab1], sab1, P1Min, P1Max, LabelXX1][[SMsigma]]] }, 
                                  {Style["ghbb[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghbbcode, fn2], Hold[Expression]],"ghbb", Left], SpanFromAbove}, 
                                  {Style["ghtautau[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghtautaucode, fn3], Hold[Expression]],"ghtautau", Left], SpanFromAbove},
                                  {Style["ghWWcode[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghWWcode, fn4], Hold[Expression]],"ghWW", Left], SpanFromAbove},
                                  {Style["ghZZcode[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghZZcode, fn5], Hold[Expression]],"ghZZ", Left], SpanFromAbove},
                                  {Style["Enter parameters values",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[a1]], "a1", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[Att]], "Att", Left], SpanFromAbove},         
                                  {Labeled[InputField[Dynamic[Cab]], "Cab", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[tb]], "tb", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[sab1]], "sab", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[P1Min]], "P1Min", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[P1Max]], "P1Max", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[SMsigma]], "SMsigma", Left], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[LabelXX1]], "LabelXX", Left], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[P1step]], "P1step", Left], SpanFromAbove},
                                  {Button[Style["Figure",Bold,12],Export[SystemDialogInput["FileSave","untitled"],FigRZone],Background->LightBlue,ImageSize->{80,40},Method->"Queued"]},
                                  {Button[Style["Table",Bold,12],Dynamic[TableRZone[ghtt[a1,Att,Cab,tb], ghbb[a1,Att,Cab,tb], ghZZ[sab1], sab1, P1Min, P1Max,P1step]],Background->LightRed,ImageSize->{80,40},Method->"Queued"]},
                                  (*{Button[Style["Clear",Bold,12],{ghttcode=Null; ghbbcode=Null; ghtautaucode=Null; ghWWcode=Null; ghZZcode=Null; a1=Null; Att=Null; Cab=Null; tb=Null; sab1=Null; P1Min =Null; P1Max =Null; SMsigma =Null; LabelXX1 =Null; P1step=Null; Graphics[{}],ghtt=Null; ghbb=Null;ghtautau=Null;ghWW=Null;ghZZ=Null},*)
                                  {Button[Style["Clear",Bold,12],{FigRZone=Graphics[{}] },Background->Lighter[Blue,0.7],ImageSize->{80,40}]} 
                                  }, Frame -> All,Alignment -> Top],
                                  tab4,"Enter couplings 2"
                                 ]
                         ], 
        	               Grid[
                                 {
                                  {Style["                                                                                            SpaceMath v.1.0                                                                                         ", "Title", FontSize -> 30, FontColor -> Black, FontWeight -> Normal], 
                                  SpanFromLeft,
                                  Import[FileNameJoin[{$SpaceMathDirectory, "Miscellaneous", "SpaceMathLogo.jpg"}]]}
                                 }, Frame -> All,Alignment -> Top],  
               TabView[
                       {
                        "Start"->Column[tabNumber=tab1;{Row[{"Welcome!"}]}],
                        "Tutorial"->Column[tabNumber=tab2;{Row[{"Guide to using the workspace"}]}],
                        "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)"->Column[tabNumber=tab3;{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\[Sigma]"}]}],
                        "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)"->Column[tabNumber=tab4;{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\[Sigma]"}]}]
                       },Dynamic@tabNumber],
              {{tabNumber,1},None},  
			  {{tab1,1},None},
			  {{tab2,2},None},
			  {{tab3,3},None},
			  {{tab4,4},None},
      		 Initialization :> {fn1[ghttcode],fn2[ghbbcode],fn3[ghtautaucode],fn4[ghWWcode],fn5[ghZZcode]}
      		] (*Termina manipulate y RZone*)
      	   ]
     	  ], Background->Lighter[Black, 0.85]
];

 WinTestXX[]:=
 Panel[
DynamicModule[
	      {
	       ghttcode = Hold[ghtt[a_,Att_,Cab_,tb_] :=(g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att)))], 
  		   ghbbcode = Hold[ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)))],
		   ghtautaucode = Hold[ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)))],
		   ghWWcode = Hold[ghWW[sab1_] := gw*mW*sab1],
		   ghZZcode = Hold[ghZZ[sab1_] := gz*mZ*sab1]
	      },
  	      With[
  	           {
  	            fn1 = (ghttcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn2 = (ghbbcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn3 = (ghtautaucode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn4 = (ghWWcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn5 = (ghZZcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &
  	           }, 
   	       Manipulate[
   	         Dynamic[                                             
                  Switch[
                         tabNumber,
                         tab1,"Enter couplings 2",
                         tab2,"Enter couplings 2",
                         tab3,"Enter couplings 2",
                         tab4,"Enter couplings 2"
                                 ]
                         ], 
        	               Grid[
                                 {
                                  {Style["                                                                                            SpaceMath v.1.0                                                                                         ", "Title", FontSize -> 30, FontColor -> Black, FontWeight -> Normal], 
                                  SpanFromLeft,
                                  Import[FileNameJoin[{$SpaceMathDirectory, "Miscellaneous", "SpaceMathLogo.jpg"}]]}
                                 }, Frame -> All,Alignment -> Top],  
               TabView[
                       {
                        "Start"->Column[tabNumber=tab1;{Row[{"Welcome!"}]}],
                        "Tutorial"->Column[tabNumber=tab2;{Row[{"Guide to using the workspace"}]}],
                        "UNO"->Column[tabNumber=tab3;{Row[{"UNO UNO"}]}],
                        TabView[{x, y}]->Column[tabNumber=tab4;{Row[{"DOS DOS"}]}]
                       },Dynamic@tabNumber],
              {{tabNumber,1},None},  
			  {{tab1,1},None},
			  {{tab2,2},None},
			  {{tab3,3},None},
			  {{tab4,4},None},
      		 Initialization :> {fn1[ghttcode],fn2[ghbbcode],fn3[ghtautaucode],fn4[ghWWcode],fn5[ghZZcode]}
      		] (*Termina manipulate*)
      	   ]
     	  ], Background->Lighter[Black, 0.85]
];
(*********************************************************************************************************************************)		  
(*********************************************************************************************************************************)  				   
WinSMA1[]:=
Panel[
DynamicModule[
	      {
	       ghttcode = Hold[ghtt[a_,Att_,Cab_,tb_] :=(g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att)))], 
  		 ghbbcode = Hold[ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)))],
		   ghtautaucode = Hold[ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)))],
		   ghWWcode = Hold[ghWW[sab1_] := gw*mW*sab1],
		   ghZZcode = Hold[ghZZ[sab1_] := gz*mZ*sab1],
	       a1 = ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
	       Att = 1,
	       Cab = Sqrt[1 - sab^2],
	       tb = 1,
	       sab1 = sab, 
	       P1Min = -1, 
	       P1Max = 1,
	       SMsigma = 1,
	       LabelXX1 = LabelXX,
	       P1step=0.001,
	       MainMenuOptionTAB1,
	       MainMenuOptionTAB2,
	       MainMenuOptionTAB3
	      },
  	      With[
  	           {
  	            fn1 = (ghttcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn2 = (ghbbcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn3 = (ghtautaucode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn4 = (ghWWcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &,
  	            fn5 = (ghZZcode = #;ReleaseHold[# /. RuleDelayed -> SetDelayed]) &
  	           }, 
   	       Manipulate[
   	         Dynamic[                                             
                  Switch[
                         tabNumber,
                         tab1,
                             Grid[
                                 {
                                  {Hyperlink[Button[Style["Documentation Center", FontSize -> 14, FontColor -> Black, FontWeight -> Normal], Background -> Lighter[Brown, 0.5], ImageSize -> {140, 50}, Method -> "Queued"], "paclet:SpaceMath/tutorial/SpaceMathOverview"],
                                   DisplayForm@ButtonBox[Style["Examples",FontSize -> 14], ButtonFunction :> SystemOpen[FileNameJoin[{$SpaceMathDirectory, "Examples"}]], Evaluator -> Automatic, Method -> "Preemptive", Appearance -> "DialogBox", Background -> Lighter[Brown, 0.5], ImageSize -> {100, 35}],
                                   Button[Style["Cite", FontSize -> 14], CreateWindow[DialogNotebook[{Button[Style["BibTex"], 
                                     CreateWindow[DialogNotebook[{CopyText1 = TextCell["@article{Arroyo-Urena:2020qup, 
                                     author = \"Arroyo-Ure\~{n}a, M.A. and Gait\'an, R. and Valencia-P\'erez, T.A.\",
                                     title = \"{$\ \\texttt{SpaceMath}$ version 1.0. $\\\\$ A $\\texttt{Mathematica}$ package for beyond the standard model parameter space searches}\",
                                     eprint = \ \"2008.00564\", archivePrefix\ = \"arXiv\", primaryClass \ = \"hep-ph\",
                                     month = \"8\", year = \ \"2020\"}"], 
                                     Button["Copy to clipboard", CopyToClipboard[CopyText1]], Button["Close", DialogReturn[]]}]], Background -> Lighter[Brown, 0.5], ImageSize -> {60, 25}], 
                                     Button[Style["Bibitem"], CreateWindow[DialogNotebook[{CopyText2 = TextCell["%\\cite{Arroyo-Urena:2020qup}
                                     \\bibitem{Arroyo-\ Urena:2020qup} M. A. Arroyo-\ Ure\~{n}a, R. Gait\'an and T. A. Valencia-P\'erez,
                                     %``$\\texttt{\ SpaceMath}$ version 1.0. $\\\\$ A $\\texttt{Mathematica}$ package for beyond the standard model parameter space searches,'' [arXiv:2008.\ 00564 [hep-ph]]."], 
                                     Button["Copy to clipboard", CopyToClipboard[CopyText2]], Button["Close", DialogReturn[]]}]], Background -> Lighter[Brown, 0.5], ImageSize -> {60, 25}],
                                     Button["Close", DialogReturn[]]}]], Background -> Lighter[Brown, 0.5], ImageSize -> {60, 25}]
                                  ,4},
                                  {"SpaceMath version 1.0. A Mathematica package for beyond the standard model parameter space searches",
                                  SpanFromLeft,SpanFromLeft,SpanFromLeft},
                                  {Style["Authors:\n  \[CirclePlus] M. A. Arroyo-Ureña\n Facultad de Estudios Superiores-Cuautitlán, Universidad Nacional Autónoma de México\n \
                                          \[CircleTimes] T. A. Valencia-Pérez\n Instituto de Física, Universidad Nacional Autónoma de México\n Contact us:	\
                                          spacemathapp@gmail.com", "Text", FontSize -> 16, FontColor -> Black],
                                   SpanFromLeft,SpanFromLeft,SpanFromLeft}
                                  }, Frame -> All,Alignment -> Top],
                         tab2,
                               Grid[
                                 {
                                  {"Tutorial",2,3,
                                  Hyperlink[Button[Style["Documentation Center", FontSize -> 14, FontColor -> Black, FontWeight -> Normal], Background -> Lighter[Brown, 0.5], ImageSize -> {140, 50}, Method -> "Queued"], "paclet:SpaceMath/tutorial/SpaceMathOverview"]
                                  },
                                  {1,"Tutorial",3,
                                  DisplayForm@ButtonBox[Style["Examples",FontSize -> 14], ButtonFunction :> SystemOpen[FileNameJoin[{$SpaceMathDirectory, "Examples"}]], Evaluator -> Automatic, Method -> "Preemptive", Appearance -> "DialogBox", Background -> Lighter[Brown, 0.5], ImageSize -> {100, 35}]
                                  },
                                  {1,2,"Tutorial",4}
                                 }, Frame -> All,Alignment -> Top],
                         tab3,
                            Grid[
                                 {
                                  {Style["HIGGS BOSON DATA","Subtitle"],SpanFromLeft},
                                  {Style["Signal Strengths \!\(\*FormBox[SubscriptBox[\(R\), \(X\)], TraditionalForm]\)",Bold,Large], SpanFromAbove},
                                  {Style["Enter couplings",Bold,Medium], SpanFromAbove},
                                  {Style["ghtt[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghttcode, fn1], Hold[Expression]],"ghtt", Left], FigRZone = Dynamic[ RZone[ghtt[a1,Att,Cab,tb], ghbb[a1,Att,Cab,tb], ghZZ[sab1], sab1, P1Min, P1Max, LabelXX1][[SMsigma]]] }, 
                                  {Style["ghbb[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghbbcode, fn2], Hold[Expression]],"ghbb", Left], SpanFromAbove}, 
                                  {Style["ghtautau[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghtautaucode, fn3], Hold[Expression]],"ghtautau", Left], SpanFromAbove},
                                  {Style["ghWWcode[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghWWcode, fn4], Hold[Expression]],"ghWW", Left], SpanFromAbove},
                                  {Style["ghZZcode[x1_,x2_,x3_,x4_]",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[ghZZcode, fn5], Hold[Expression]],"ghZZ", Left], SpanFromAbove},
                                  {Style["Enter parameters values",Bold,Medium], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[a1]], "a1", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[Att]], "Att", Left], SpanFromAbove},         
                                  {Labeled[InputField[Dynamic[Cab]], "Cab", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[tb]], "tb", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[sab1]], "sab", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[P1Min]], "P1Min", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[P1Max]], "P1Max", Left],SpanFromAbove},
                                  {Labeled[InputField[Dynamic[SMsigma]], "SMsigma", Left], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[LabelXX1]], "LabelXX", Left], SpanFromAbove},
                                  {Labeled[InputField[Dynamic[P1step]], "P1step", Left], SpanFromAbove},
                                  {Button[Style["Figure",Bold,12],Export[SystemDialogInput["FileSave","untitled"],FigRZone],Background->LightBlue,ImageSize->{80,40},Method->"Queued"]},
                                  {Button[Style["Table",Bold,12],Dynamic[TableRZone[ghtt[a1,Att,Cab,tb], ghbb[a1,Att,Cab,tb], ghZZ[sab1], sab1, P1Min, P1Max,P1step]],Background->LightRed,ImageSize->{80,40},Method->"Queued"]},
                                  (*{Button[Style["Clear",Bold,12],{ghttcode=Null; ghbbcode=Null; ghtautaucode=Null; ghWWcode=Null; ghZZcode=Null; a1=Null; Att=Null; Cab=Null; tb=Null; sab1=Null; P1Min =Null; P1Max =Null; SMsigma =Null; LabelXX1 =Null; P1step=Null; Graphics[{}],ghtt=Null; ghbb=Null;ghtautau=Null;ghWW=Null;ghZZ=Null},*)
                                  {Button[Style["Clear",Bold,12],{FigRZone=Graphics[{}] },Background->Lighter[Blue,0.7],ImageSize->{80,40}]} 
                                  }, Frame -> All,Alignment -> Top],
                                  tab4,"Enter couplings 2"
                                 ]
                         ], 
        	               Grid[
                                 {
                                  {Style["                                                                                            SpaceMath v.1.0                                                                                         ", "Title", FontSize -> 30, FontColor -> Black, FontWeight -> Normal], 
                                  SpanFromLeft,
                                  Import[FileNameJoin[{$SpaceMathDirectory, "Miscellaneous", "SpaceMathLogo.jpg"}]]}
                                 }, Frame -> All,Alignment -> Top],  
               TabView[
                       {
                        "Start"->Column[tabNumber=tab1;{Row[{"Welcome!"}]}],
                        "Tutorial"->Column[tabNumber=tab2;{Row[{"Guide to using the workspace"}]}],
                        "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)"->Column[tabNumber=tab3;{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\[Sigma]"}]}],
                        "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)"->Column[tabNumber=tab4;{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\[Sigma]"}]}]
                       },Dynamic@tabNumber],
              {{tabNumber,1},None},  
			  {{tab1,1},None},
			  {{tab2,2},None},
			  {{tab3,3},None},
			  {{tab4,4},None},
      		 Initialization :> {fn1[ghttcode],fn2[ghbbcode],fn3[ghtautaucode],fn4[ghWWcode],fn5[ghZZcode]}
      		] (*Termina manipulate y RZone*)
      	   ]
     	  ], Background->Lighter[Black, 0.85]
];

End[]
