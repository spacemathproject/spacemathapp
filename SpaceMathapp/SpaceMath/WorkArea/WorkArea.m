(* Wolfram Language Package *)

WinRXX::usage="WinRXX"

WinArea::usage="WinArea"

WinRZone::usage="WinRZone"

WinNorm::usage="WinNorm"

WinTest::usage="WinTest"

WinTest1::usage="WinTest1"

WinTest2::usage="WinTest2"

WinTest3Original::usage="WinTest3"

WinTest4::usage="WinTest4"

WinTest6::usage="WinTest6"

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
    				   
WinRXX[]:=  DynamicModule[
	      {
	       ghtt = Null, 
	       ai = Null, 
	       Atti = Null,
	       tbi = Null,
	       Cabi = Null
	      }, 
	      Deploy @ Panel @ Grid @ Transpose @
    	      { 
    	       { 
    	        "Coupling:", 
    	        "a:", "Att", "tb", "Cab",
    	        "slope:"
    	       }, 
    	       { 
    		InputField[Dynamic[ghtt], Boxes], 
    		InputField[Dynamic[ai]], 
    		InputField[Dynamic[Atti]], 
    		InputField[Dynamic[tbi]],   
    		InputField[Dynamic[Cabi]],     		    		  		
    		InputField[Dynamic[ToExpression@ghtt /. {a -> ai, Att -> Atti, tb -> tbi, Cab -> Cabi}], Enabled -> False]
               }
              }
             ];	
             
WinNorm[x_] := CreateWindow[
  					    DialogNotebook[
  						 {
  						  TextCell["Work area "], 
    						Module[
   		 					 {fun1, fun2},
   		 					 fun1[p_] := p^2 + p - 1;
   		 					 fun2[p_] := p^3 - p^2 + p + 1;
   		 				 	 Max[fun1[x], fun2[x]]
   						    ]
    				     }
    				    ]
    				   ];   
    				   
WinTest1[] := CreateWindow[
  					    DialogNotebook[
  						 {
  						  TextCell["Work area "], 
    						Module[
   		 					 {ghtt, ghbb,ghtautau,ghWW,ghZZ},
	       					   ghtt[a_,Att_,Cab_,tb_] := (g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att))); 
		   					   ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)));
		   					   ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)));
		   					   ghWW[sab_] := gw*mW*sab;
		   					   ghZZ[sab_] := gz*mZ*sab;
							 RZone[
      					  		 ghtt[
           						  ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						  1, 
           						  Sqrt[1 - sab^2],
           						  1
          						  ], 
      					  		 ghbb[
           						  ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						  1, 
           						  Sqrt[1 - sab^2], 
           						  1
          						 ], 
      					  		ghZZ[sab], 
      					  		sab, -1, 1, "sin(\[Alpha]-\[Beta])"
      					 	   ][[2]]   		 				 	 
   						    ]
    				     }
    				    ]
    				   ];   
    				      				   
WinTest2[] := DynamicModule[
			    {
			     ghtt, 
			     ghbb,
			     ghtautau,
			     ghWW,
			     ghZZ,
			     var1 = Null,
			     var2 = Null,
			     var3 = Null,
			     var4 = Null,	 	  
			     Coupling1 = Null,
			     value1 = Null, 
			     value2 = Null,
			     value3 = Null,
			     value4 = Null
			     },
			    ghtt[a_,Att_,Cab_,tb_] := (g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att))); 
		   	    ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)));
		   	    ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)));
		   	    ghWW[sab_] := gw*mW*sab;
		   	    ghZZ[sab_] := gz*mZ*sab;	
      			    Panel[
      	    			  Grid[
			     	       {
		         	    	{Style["Coupling", Bold], SpanFromLeft}, 
		         	    	{"Coupling:", InputField[Dynamic[Coupling1], Boxes, FieldSize -> {{20, Infinity}, {1, Infinity}}, FieldHint -> "Enter a coupling"]},       	    	  
		         	    	{"a:", InputField[Dynamic[var1], Boxes, FieldHint -> "Enter a parameter"]},
		        	    	{"ai:", InputField[Dynamic[value1], Boxes, FieldHint -> "Enter a value"]},      	    	  
			     	    	{"Att:", InputField[Dynamic[var2], Boxes, FieldHint -> "Enter a parameter"]},
			      	    	{"Atti:", InputField[Dynamic[value2], Boxes, FieldHint -> "Enter a value"]},      	    	  
			      	    	{"tb:", InputField[Dynamic[var3], Boxes, FieldHint -> "Enter a parameter"]},
			      	    	{"tbi:", InputField[Dynamic[value3], Boxes, FieldHint -> "Enter a value"]},
			      	    	{"Cab:", InputField[Dynamic[var4], Boxes, FieldHint -> "Enter a parameter"]},
			      	    	{"Cabi:", InputField[Dynamic[value4], Boxes, FieldHint -> "Enter a value"]}     	    	       	    	        	    	        	    	       	    	   	    	 	       
			      	       },
			      	       {
			      	        Dynamic[RZone[
      					  	      ghtt[
           						   ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						   1, 
           						   Sqrt[1 - sab^2],
           						   1
          						  ], 
      					  	      ghbb[
           						   ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						   1, 
           						   Sqrt[1 - sab^2], 
           						   1
          						  ], 
      					  	      ghZZ[sab], 
      					  	      sab, -1, 1, "sin(\[Alpha]-\[Beta])"
      					 	    ][[2]]]
			      	       }
      	    	                      ]
      	                          ]
		   	    ];      
		   	    
WinTest3Original[] := Panel[
                      Manipulate[
                            Dynamic[
                                If[
                                    AllTrue[{P1Min, P1Max},NumericQ],                                                    
                                    Switch[
                                              tabNumber,
                                              tab1,
                                              DynamicModule[
                                                                    {Vbb = Null},
                                                                    Column[
                                                                               {
                                                                                 Style["Coupling Type Vbb"],
                                                                                 InputField[Dynamic[Vbb], String],
                                                                                 KappaFigE = Dynamic[KappaX[ToExpression[Vbb, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 1]]
                                                                                }
                                                                               ]
                                                                    ],
                                              tab2,
                                              DynamicModule[
      																{Vbb = Null},
      																Column[
      																		   {
        																		 Style["Coupling Type Vbb"],
        																		 InputField[Dynamic[Vbb], String],
        																		 KappaFigE = Dynamic[KappaX[ToExpression[Vbb, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 2]]
        																		}
       																		   ]
      																],
                         					  tab3,
     										  DynamicModule[
      																{Vtt = Null},
      																Column[
       																			{
        																		  Style["Coupling Type Vtt"],
        																		  InputField[Dynamic[Vtt], String],
        																		  KappaFigE = Dynamic[KappaX[ToExpression[Vtt, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 3]]
                                                                                }
       																		   ]
      																],
                        				      tab4,
     										  DynamicModule[
      																{Vtt = Null},
      																Column[
       																			{
        																		  Style["Coupling Type Vtt"],	
        																		  InputField[Dynamic[Vtt], String],
        																		  KappaFigE = Dynamic[KappaX[ToExpression[Vtt, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 4]]
        																		}
       																		   ]
      																],
                        				      tab5,
     										  DynamicModule[
      																{Vtautau = Null},
      																Column[
       																			{
        																		  Style["Coupling Type Vtautau"],
        																		  InputField[Dynamic[Vtautau], String],
        																		  KappaFigE = Dynamic[KappaX[ToExpression[Vtautau, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 5]]
        																		}
       																		   ]
      																],
                        					  tab6,
     										  DynamicModule[
      																{Vtautau = Null},
      																Column[
       																			{
        																		  Style["Coupling Type Vtautau"],
        																		  InputField[Dynamic[Vtautau], String],
        																		  KappaFigE = Dynamic[KappaX[ToExpression[Vtautau, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 6]]
        																		}
       																		   ]
      																],
                        					  tab7,
     										  DynamicModule[
      																{VWW = Null},
      																Column[
       																			{
        																		  Style["Coupling Type VWW"],
        																		  InputField[Dynamic[VWW], String],
        																		  KappaFigE = Dynamic[KappaX[ToExpression[VWW, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 7]]
        																		}
       																		   ]
      																],
                        					  tab8,
     										  DynamicModule[
      																{VWW = Null},
      																 Column[
       																			{
        																		  Style["Coupling Type VWW"],
        																	      InputField[Dynamic[VWW], String],
        																		  KappaFigE = Dynamic[KappaX[ToExpression[VWW, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 8]]
        																		}
       																		   ]
      																],
                        					  tab9,
     									  	  DynamicModule[
      																{Vzz = Null},
      																Column[
       																		   {
        																		Style["Coupling Type Vzz"],
        																		InputField[Dynamic[Vzz], String],
        																		KappaFigE = Dynamic[KappaX[ToExpression[Vzz, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 9]]
        																	   }
       																		  ]
      																],
                      						  tab10,
     										  DynamicModule[
      																{Vzz = Null},
      																Column[
       																		   {
        																		Style["Coupling Type Vzz"],
        																		InputField[Dynamic[Vzz], String],
        																		KappaFigE = Dynamic[KappaX[ToExpression[Vzz, TraditionalForm], P1, P1Min, P1Max, variablenameP1, 10]]
        																	   }
       																		  ]
      																]
                    				      ],
          						    Graphics[{}]
       							  ]
    						  ],
  					        Grid[
   								   {
    								{Style["SpaceMath", "Title"], SpanFromLeft, SpanFromLeft},
    								{Style["HIGGS BOSON DATA", "Subtitle"], SpanFromLeft,SpanFromLeft},
    								{Style["\[Kappa]-parametrization", Gray, Bold, 15]},
    								{
     								  TabView[
                                                  {
                          							"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab1; 
                          							{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 1\\[Sigma]"}]}],
                                                    "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab2; 
                                                    {Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)" -> Column[tabNumber = tab3; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\) at 1\\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)" -> Column[tabNumber = tab4; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\) at 2\\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)" -> Column[tabNumber = tab5; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\) \at 1\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)" -> Column[tabNumber = tab6; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\) \at 2\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)" -> Column[tabNumber = tab7; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\) at 1\\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)" -> Column[tabNumber = tab8; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\) at 2\\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)" -> Column[tabNumber = tab9; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\) at 1\\[Sigma]"}]}],
                   									"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)" -> Column[tabNumber = tab10; 
                   									{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\) at \2\[Sigma]"}]}]
                  								  },
                  								 Dynamic@tabNumber
                  								]
     								},
    						{
     						  Control@{
     						  				{variablenameP1, Null, Style["Model Parameter name P1", Bold, 12]},
     						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
     						  			  }, 
     						  Button[
     						  	 		Style["Save", Bold, 12],
     						  	 		ToExpression[variablenameP1, StandardForm,Function[nameP1, nameP1 = P1, HoldFirst]], 
     						  	 		Background -> Lighter[Blue, 0.7], 
     						  	 		ImageSize -> 80
     						  	 	   ]
     						},
    						{
     						  Control@{
     						  				{P1Min, Null, Style["P1Min", Bold, 12]}, 
       						  				InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }, 
     						  Control@{
     						  			    {P1Max, Null, Style["P1Max", Bold, 12]}, 
       										InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       									   }
     						},
    					    {
     						  Control@{
     						  				{variablenameQA1, Null, Style["Parameter Fixed 1", Bold, 12]}, 
       										InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       									   }, 
     						  Control@{
     						  			    {QA1, Null, Style["Value", Bold, 12]}, 
     						  			    InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
     						  			   }, 
     						  Button[
     						  			Style["Save", Bold, 12],
     						  			ToExpression[variablenameQA1, StandardForm, Function[nameQA1, nameQA1 = QA1, HoldFirst]], 
     						  			Background -> Lighter[Blue, 0.7], 
     						  			ImageSize -> 80
     						  		   ]
     						},
    					    {
     						  Control@{
     						  				{variablenameQA2, Null, Style["Parameter Fixed 2", Bold, 12]},
     						  				InputField[#, String, Background -> Lighter[Gray, 0.7], ImageSize -> 80] &
     						  			  }, 
     						  Control@{
     						  				{QA2, Null, Style["Value", Bold, 12]},
     						  				InputField[#, Number, Background -> Lighter[Gray, 0.7], ImageSize -> 80] &
     						  			   }, 
     						  Button[
     						  			Style["Save", Bold, 12],
     						  			ToExpression[variablenameQA2, StandardForm,Function[nameQA2, nameQA2 = QA2, HoldFirst]], 
      									Background -> Lighter[Blue, 0.7],
      									ImageSize -> 80
      								  ]
     						},
    						{
     						  Control@{
     						  				{variablenameQA3, Null, Style["Parameter Fixed 3", Bold, 12]},
     						  				InputField[#, String, Background -> Lighter[Gray, 0.7], ImageSize -> 80] &
     						  			   },
     						 Control@{
     						 			   {QA3, Null, Style["Value", Bold, 12]},
     						 			   InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
     						 			  },
     						 Button[
     						 		   Style["Save", Bold, 12],
     						 		   ToExpression[variablenameQA3, StandardForm,Function[nameQA3, nameQA3 = QA3, HoldFirst]],
     						 		   Background -> Lighter[Blue, 0.7], 
     						 		   ImageSize -> 80
     						 		  ]
     					    },
     					    {
     					      Button[
     					      			Style["Clear", Bold, 12],
     					      			{
     					      			  Vbb = Null,
     					      			  Vtt = Null,
     					      			  Vtautau = Null,
     					      			  VWW = Null,
     					      			  Vzz = Null,
     					      			  P1 = Null,
     					      			  variablenameP1 = Null,
     					      			  P1Min = Null,
     					      			  P1Max = Null,
     					      			  QA1 = Null,
     					      			  variablenameQA1 = Null,
     					      			  QA2 = Null,
     					      			  variablenameQA2 = Null,
     					      			  QA3 = Null,
     					      			  variablenameQA3 = Null,
     					      			  Graphics[{}]
     					      			},
     					      			Background -> LightRed, 
     					      			ImageSize -> {80, 40}
     					      		  ],
     					      Button[
     					      			Style["Export", Bold, 12],
     					      			Export[SystemDialogInput["FileSave", "untitled"], KappaFigE],
     					      			Background -> LightBlue, 
     					      			ImageSize -> {80, 40},
     					      			Method -> "Queued"
     					      		   ]
     						}
    					   }, Spacings -> {2, 1}, Background -> Lighter[White, 0.8]
   						 ],
  						{{tabNumber, 1}, None},  
  						{{tab1, 1}, None},
  						{{tab2, 2}, None},
  						{{tab3, 3}, None},
  						{{tab4, 4}, None},
  						{{tab5, 5}, None},
  						{{tab6, 6}, None},
  						{{tab7, 7}, None},
  						{{tab8, 8}, None},
  						{{tab9, 9}, None},
  						{{tab10, 10}, None},
  						ControlPlacement -> Left, 
  						SaveDefinitions -> True
  					   ], Background -> Lighter[Black, 0.5]
  					 ];		
  					 
WinTest4Original[] := Panel[
                      Manipulate[
                            Dynamic[
                                If[
                                    AllTrue[{P1Min, P1Max,SMsigma},NumericQ],                                                    
                                    Switch[
                                              tabNumber,
                                              tab1,
                                              DynamicModule[
                                                                    {
                                                                   	  ghtt, 
		   															  ghbb,
		   															  ghtautau,
		   															  ghWW,
		   															  ghZZ
                                                                    },
                                                                   	  ghtt[a_,Att_,Cab_,tb_] := (g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att)));
		   															  ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)));
		   															  ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)));
		   															  ghWW[sab_] := gw*mW*sab;
		   															  ghZZ[sab_] := gz*mZ*sab;                                                                    
                                                                    Column[
                                                                               {
                                                                                 Style["Coupling Type Vbb"],
                                                                                 KappaFigE = Dynamic[
                                                                                 					RZone[
      					  																					  ghtt[
            						 																				 ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						 																					 1, 
           						 																					 Sqrt[1 - sab^2],
           						 																					 1
          																											], 
      					  																					  ghbb[
           						  																					  ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						  																					  1, 
           						  																					  Sqrt[1 - sab^2], 
           						  																					  1
          						 																					 ], 
      					  																					  ghZZ[sab], 
      					  																					  sab, P1Min, P1Max, "sin(\[Alpha]-\[Beta])"
      					 																					 ]
      				    																				[[SMsigma]]
                                                                                 	                  ]
                                                                                }
                                                                               ]
                                                                    ],
                                              tab2,
												Style["Coupling Type Vbb"]
                    				      ],
          						    Graphics[{}]
       							  ]
    						  ],
  					        Grid[
   								   {
    								{Style["SpaceMath", "Title"], SpanFromLeft, SpanFromLeft},
    								{Style["HIGGS BOSON DATA", "Subtitle"], SpanFromLeft,SpanFromLeft},
    								{Style["\[Kappa]-parametrization", Gray, Bold, 15]},
    								{
     								  TabView[
                                                  {
                          							"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab1; 
                          							{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 1\\[Sigma]"}]}],
                                                    "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab2; 
                                                    {Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\\[Sigma]"}]}]
                  								  },
                  								 Dynamic@tabNumber
                  								]
     								},
    						{
     						  Control@{
     						  				{variablenameP1, Null, Style["Model Parameter name P1", Bold, 12]},
     						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
     						  			  }, 
     						  Button[
     						  	 		Style["Save", Bold, 12],
     						  	 		ToExpression[variablenameP1, StandardForm,Function[nameP1, nameP1 = P1, HoldFirst]], 
     						  	 		Background -> Lighter[Blue, 0.7], 
     						  	 		ImageSize -> 80
     						  	 	   ]
     						},
    						{
     						  Control@{
     						  				{P1Min, Null, Style["P1Min", Bold, 12]}, 
       						  				InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }, 
     						  Control@{
     						  			    {P1Max, Null, Style["P1Max", Bold, 12]}, 
       										InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       									   },
     						  Control@{
     						  				{SMsigma, Null, Style["Sigma", Bold, 12]}, 
       						  				InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   },
     						  Control@{
     						  				{LabelXX, "LabelXX", Style["LabelXX", Bold, 12]}, 
       						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }         						  			           									   
     						}
    					   }, Spacings -> {2, 1}, Background -> Lighter[White, 0.8]
   						 ],
  						{{tabNumber, 1}, None},  
  						{{tab1, 1}, None},
  						{{tab2, 2}, None},
  						ControlPlacement -> Left
  					   ], Background -> Lighter[Black, 0.5]
  					 ];	  
  					 
WinTest5Original[] := Panel[
                      Manipulate[
                            Dynamic[
                                If[
                                    AllTrue[{P1Min, P1Max,SMsigma},NumericQ] && AllTrue[{LabelXX},StringQ],                                                                                    
                                    Switch[
                                              tabNumber,
                                              tab1,
                                              DynamicModule[
                                                                    {
                                                                   	  ghtt, 
		   															  ghbb,
		   															  ghtautau,
		   															  ghWW,
		   															  ghZZ
                                                                    },
                                                                   	  ghtt[a_,Att_,Cab_,tb_] := (g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att)));
		   															  ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)));
		   															  ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)));
		   															  ghWW[sab_] := gw*mW*sab;
		   															  ghZZ[sab_] := gz*mZ*sab;                                                                    
                                                                    Column[
                                                                               {
                                                                                 Style["Coupling Type Vbb"],
                                                                                 KappaFigE = Dynamic[
                                                                                 					RZone[
      					  																					  ghtt[
            						 																				 ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						 																					 1, 
           						 																					 Sqrt[1 - sab^2],
           						 																					 1
          																											], 
      					  																					  ghbb[
           						  																					  ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						  																					  1, 
           						  																					  Sqrt[1 - sab^2], 
           						  																					  1
          						 																					 ], 
      					  																					  ghZZ[sab], 
      					  																					  sab, P1Min, P1Max, LabelXX
      					 																					 ]
      				    																				[[SMsigma]]
                                                                                 	                  ]
                                                                                }
                                                                               ]
                                                                    ],
                                              tab2,
												Style["Coupling Type Vbb"]
                    				      ],
          						    Graphics[{}]
       							  ]
    						  ],
  					        Grid[
   								   {
    								{Style["SpaceMath", "Title"], SpanFromLeft, SpanFromLeft},
    								{Style["HIGGS BOSON DATA", "Subtitle"], SpanFromLeft,SpanFromLeft},
    								{Style["\[Kappa]-parametrization", Gray, Bold, 15]},
    								{
     								  TabView[
                                                  {
                          							"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab1; 
                          							{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 1\\[Sigma]"}]}],
                                                    "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab2; 
                                                    {Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\\[Sigma]"}]}]
                  								  },
                  								 Dynamic@tabNumber
                  								]
     								},
    						{
     						  Control@{
     						  				{variablenameP1, Null, Style["Model Parameter name P1", Bold, 12]},
     						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
     						  			  }, 
     						  Button[
     						  	 		Style["Save", Bold, 12],
     						  	 		ToExpression[variablenameP1, StandardForm,Function[nameP1, nameP1 = P1, HoldFirst]], 
     						  	 		Background -> Lighter[Blue, 0.7], 
     						  	 		ImageSize -> 80
     						  	 	   ]
     						},
    						{
     						  Control@{
     						  				{P1Min, Null, Style["P1Min", Bold, 12]}, 
       						  				InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }, 
     						  Control@{
     						  			    {P1Max, Null, Style["P1Max", Bold, 12]}, 
       										InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       									   },
     						  Control@{
     						  				{SMsigma, Null, Style["Sigma", Bold, 12]}, 
       						  				InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   },
     						  Control@{
     						  				{LabelXX, "LabelXX", Style["LabelXX", Bold, 12]}, 
       						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }         						  			           									   
     						}
    					   }, Spacings -> {2, 1}, Background -> Lighter[White, 0.8]
   						 ],
  						{{tabNumber, 1}, None},  
  						{{tab1, 1}, None},
  						{{tab2, 2}, None},
  						ControlPlacement -> Left
  					   ], Background -> Lighter[Black, 0.5]
  					 ];	  	
  					 
WinTest6[] := Panel[
                      Manipulate[
                            Dynamic[
                                If[
                                    AllTrue[{P1Min, P1Max,SMsigma},NumericQ] && AllTrue[{LabelXX,ghttParameter1},StringQ],                                                                                    
                                    Switch[
                                              tabNumber,
                                              tab1,
                                              DynamicModule[
                                                                    {
                                                                   	  ghtt, 
		   															  ghbb,
		   															  ghtautau,
		   															  ghWW,
		   															  ghZZ
                                                                    },
                                                                   	  ghtt[a_,Att_,Cab_,tb_] := (g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att)));
		   															  ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb)));
		   															  ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata)));
		   															  ghWW[sab_] := gw*mW*sab;
		   															  ghZZ[sab_] := gz*mZ*sab;                                                                    
                                                                    Column[
                                                                               {
                                                                                 Style["Coupling Type Vbb"],
                                                                                 KappaFigE = Dynamic[
                                                                                 					RZone[
      					  																					  ghtt[
           						  																					  ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						  																					  1, 
           						  																					  Sqrt[1 - sab^2], 
           						  																					  1
          																											], 
      					  																					  ghbb[
           						  																					  ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						  																					  1, 
           						  																					  Sqrt[1 - sab^2], 
           						  																					  1
          						 																					 ], 
      					  																					  ghZZ[ToExpression[ghttParameter1,TraditionalForm]], 
      					  																					  sab, 
      					  																					  P1Min, 
      					  																					  P1Max, 
      					  																					  LabelXX
      					 																					 ]
      				    																				[[SMsigma]]
                                                                                 	                  ]
                                                                                }
                                                                               ]
                                                                    ],
                                              tab2,
												Style["Coupling Type Vbb"]
                    				      ],
          						    Graphics[{}]
       							  ]
    						  ],
  					        Grid[
   								   {
    								{Style["SpaceMath", "Title"], SpanFromLeft, SpanFromLeft},
    								{Style["HIGGS BOSON DATA", "Subtitle"], SpanFromLeft,SpanFromLeft},
    								{Style["\[Kappa]-parametrization", Gray, Bold, 15]},
    								{
     								  TabView[
                                                  {
                          							"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab1; 
                          							{Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 1\\[Sigma]"}]}],
                                                    "\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)" -> Column[tabNumber = tab2; 
                                                    {Row[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) at 2\\[Sigma]"}]}]
                  								  },
                  								 Dynamic@tabNumber
                  								]
     								},
    						{
     						  Control@{
     						  				{variablenameP1, Null, Style["Model Parameter name P1", Bold, 12]},
     						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
     						  			  }, 
     						  Button[
     						  	 		Style["Save", Bold, 12],
     						  	 		ToExpression[variablenameP1, StandardForm,Function[nameP1, nameP1 = P1, HoldFirst]], 
     						  	 		Background -> Lighter[Blue, 0.7], 
     						  	 		ImageSize -> 80
     						  	 	   ]
     						},
    						{
     						  Control@{
     						  				{P1Min, Null, Style["P1Min", Bold, 12]}, 
       						  				InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }, 
     						  Control@{
     						  			    {P1Max, Null, Style["P1Max", Bold, 12]}, 
       										InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       									   },
     						  Control@{
     						  				{SMsigma, Null, Style["Sigma", Bold, 12]}, 
       						  				InputField[#, Number, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   },
     						  Control@{
     						  				{LabelXX, "LabelXX", Style["LabelXX", Bold, 12]}, 
       						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }         						  			           									   
     						},
    						{
     						  Control@{
     						  				{ghttParameter1, Null, Style["ghttParameter1", Bold, 12]}, 
       						  				InputField[#, String, Background -> Lighter[Gray, 0.7],ImageSize -> 80] &
       						  			   }    						  			           									   
     						}     						
    					   }, Spacings -> {2, 1}, Background -> Lighter[White, 0.8]
   						 ],
  						{{tabNumber, 1}, None},  
  						{{tab1, 1}, None},
  						{{tab2, 2}, None},
  						ControlPlacement -> Left
  					   ], Background -> Lighter[Black, 0.5]
  					 ];	  					 				 					 
  					    	       				      				   
    				    	
WinArea[] := DynamicModule[
	      {
	       ghtt[a_,Att_,Cab_,tb_] := (g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att))), 
		   ghbb[a_,Abb_,Cab_,tb_] := (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb))),
		   ghtautau[a_,Atata_,Cab_,tb_] := (g/2) (mtau/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev*Atata))),
		   ghWW[sab_] := gw*mW*sab,
		   ghZZ[sab_] := gz*mZ*sab
	      }, 
	      Panel @
    	      { 
				RZone[
      					  ghtt[
           						 ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						 1, 
           						 Sqrt[1 - sab^2],
           						 1
          						], 
      					  ghbb[
           						  ArcCos[Sqrt[1 - sab^2]] + ArcTan[1], 
           						  1, 
           						  Sqrt[1 - sab^2], 
           						  1
          						 ], 
      					  ghZZ[sab], 
      					  sab, -1, 1, "sin(\[Alpha]-\[Beta])"
      					 ]
      				    [[2]]
              }
             ];             
             
             
WinRZone[] := CreateWindow[
  					DialogNotebook[
  						{
  						  TextCell["Work area "], 
RZone[ 
	      (g/2) (mt/mW) ((Cos[a]/(tb*Cos[ArcTan[tb]])) - (Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev*Att))), 
          (g/2) (mb/mW) (((-Sin[a]*tb)/Sin[ArcTan[tb]]) + (Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev*Abb))),
          gz*mZ*sab, 
          sab, -1, 1, "sin(\[Alpha]-\[Beta])"]
        [[2]]  
    				     }
    				    ]
    				   ];   
    				    				   

End[]
