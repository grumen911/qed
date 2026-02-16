BeginPackage["QED`Scattering`", {"QED`Model`"}];

(* Публичные функции *)
BuildSymbolicScattering::usage = 
  "BuildSymbolicScattering[topology, options] строит символьную S-матрицу и матрицу проводимости (Y).
   Использует унифицированные символы: Subscript[\"C\", name] и Subscript[\"L\", name] для всех типов компонентов.
   Опции:
     Ports -> {nodeIdx1, nodeIdx2} (по умолчанию: {первый, последний})
     ReferenceImpedance -> 50 (Ом)
     IgnoreJunctionCapacitance -> False
   Возвращает ассоциацию с символьными матрицами и списком сгенерированных символов.";

ComputeNumericalScattering::usage = 
  "ComputeNumericalScattering[model, frequencyList] вычисляет численные значения S-матрицы 
   для списка частот. Автоматически связывает символы из топологии со значениями в Primary.";

GetEffectiveInductances::usage = 
  "GetEffectiveInductances[model] возвращает список правил для эффективных индуктивностей 
   джозефсоновских переходов в рабочей точке.";

Begin["`Private`"];

(* ════════════════════════════════════════════════════════════════ *)
(* СИМВОЛЬНОЕ ПОСТРОЕНИЕ (ANALYTIC)                    *)
(* ════════════════════════════════════════════════════════════════ *)

Options[BuildSymbolicScattering] = {
  Ports -> Automatic, 
  ReferenceImpedance -> 50,
  IgnoreJunctionCapacitance -> False
};

BuildSymbolicScattering[topology_Association, opts : OptionsPattern[]] := 
 Module[{
    nodes, groundNode, activeNodes, nNodes,
    components, yMatrix, portIndices, z0, ignoreCap,
    sVar, (* s = i*omega *)
    internalIndices, 
    Ypp, Ypi, Yip, Yii, Yschur, 
    unitMatrix, sMatrix,
    generatedSymbols
 },
  
  nodes = topology["Nodes"];
  groundNode = topology["GroundNode"];
  components = topology["Components"];
  
  activeNodes = Cases[nodes, Except[groundNode]];
  nNodes = Length[activeNodes];
  
  z0 = OptionValue[ReferenceImpedance];
  ignoreCap = TrueQ[OptionValue[IgnoreJunctionCapacitance]];
  
  portIndices = If[OptionValue[Ports] === Automatic,
      {1, nNodes}, 
      Flatten[FirstPosition[activeNodes, #] & /@ OptionValue[Ports]]
  ];
  
  sVar = Symbol["s"]; 
  
  yMatrix = ConstantArray[0, {nNodes, nNodes}];
  generatedSymbols = <||>;
  
  Do[
    Module[{type, n1, n2, name, u, v, yComp, symC, symL},
      {type, n1, n2, name} = comp[[1;;4]];
      
      u = If[n1 === groundNode, 0, FirstPosition[activeNodes, n1][[1]]];
      v = If[n2 === groundNode, 0, FirstPosition[activeNodes, n2][[1]]];
      
      If[u == 0 && v == 0, Continue[]];

      (* УПРОЩЕНИЕ: Используем только "C" и "L" как заголовки *)
      yComp = Switch[type,
        "Capacitor",
          symC = Subscript["C", name];
          sVar * symC,
          
        "Inductor",
          symL = Subscript["L", name];
          1 / (sVar * symL),
          
        "JosephsonJunction",
          (* Для JJ: L - это эффективная индуктивность, C - собственная емкость *)
          symL = Subscript["L", name]; 
          
          If[ignoreCap,
             1 / (sVar * symL),
             symC = Subscript["C", name];
             1 / (sVar * symL) + sVar * symC
          ],
          
        _, 0
      ];
      
      (* Заполнение матрицы *)
      If[u > 0, yMatrix[[u, u]] += yComp];
      If[v > 0, yMatrix[[v, v]] += yComp];
      If[u > 0 && v > 0, 
        yMatrix[[u, v]] -= yComp;
        yMatrix[[v, u]] -= yComp;
      ];
    ],
    {comp, components}
  ];

  (* Редукция (Schur Complement) *)
  internalIndices = Complement[Range[nNodes], portIndices];
  
  If[Length[internalIndices] > 0,
    Ypp = yMatrix[[portIndices, portIndices]];
    Yii = yMatrix[[internalIndices, internalIndices]];
    Ypi = yMatrix[[portIndices, internalIndices]];
    Yip = yMatrix[[internalIndices, portIndices]];
    
    Yschur = Simplify[Ypp - Ypi . Inverse[Yii] . Yip];
  ,
    Yschur = yMatrix[[portIndices, portIndices]];
  ];

  (* S-матрица *)
  unitMatrix = IdentityMatrix[Length[portIndices]];
  
  sMatrix = Simplify[
    (unitMatrix - z0 * Yschur) . Inverse[unitMatrix + z0 * Yschur]
  ];

  <|
    "YMatrixFull" -> yMatrix,
    "YMatrixReduced" -> Yschur,
    "SMatrix" -> sMatrix,
    "FrequencyVariable" -> sVar,
    "PortIndices" -> portIndices,
    "ActiveNodes" -> activeNodes
  |>
 ];


(* ════════════════════════════════════════════════════════════════ *)
(* ЧИСЛЕННАЯ ПОДСТАНОВКА (NUMERICAL)                   *)
(* ════════════════════════════════════════════════════════════════ *)

GetEffectiveInductances[model_Association] := 
 Module[{topology, primary, subRules, components, groundNode, 
        phiMin, phi0Val},
  
  topology = model["Topology"];
  primary = model["Primary"]; 
  components = topology["Components"];
  subRules = model["SubstitutionRules"];
  groundNode = topology["GroundNode"];
  phi0Val = QED`$Phi0Value; 
  
  phiMin = QED`Model`GetNumericalQuantity[model, "EquilibriumFluxes"];
  If[phiMin === $Failed, Return[$Failed]];
  
  Cases[components, 
    {type_, n1_, n2_, name_, ___} /; type === "JosephsonJunction" :> 
     Module[{params, ejVal, phi1, phi2, phiDiff, phaseDrop, cosPhi, lVal},
         
         params = primary[name];
         ejVal = (params["EJ"]["Symbol"] /. subRules); 
         
         phi1 = If[n1 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n1] /. phiMin];
         phi2 = If[n2 === groundNode, 0., Subscript[QED`$FluxSymbol, "min", n2] /. phiMin];
         
         phiDiff = (phi1 - phi2); 
         phaseDrop = 2 * Pi * phiDiff / phi0Val;
         cosPhi = Cos[phaseDrop];
         
         If[cosPhi <= 10^-6, cosPhi = 10^-6]; 

         lVal = (phi0Val / (2 * Pi))^2 / (ejVal * cosPhi);
         
         (* УПРОЩЕНИЕ: Теперь мы возвращаем правило для Subscript["L", name] *)
         Subscript["L", name] -> lVal
     ]
  ]
 ];


ComputeNumericalScattering[model_Association, frequencyList_List] := 
 Module[{symScattering, components, primary, valRules, lJeffRules, 
        sMatrixSym, sVarSym, sMatrixNumFunction, omegaList},
  
  symScattering = BuildSymbolicScattering[model["Topology"]];
  
  components = model["Topology"]["Components"];
  primary = model["Primary"];
  
  valRules = Flatten @ Map[
    Function[comp,
      Module[{type, name, p},
        {type, name} = {comp[[1]], comp[[4]]};
        p = primary[name]; 
        
        Switch[type,
          "Capacitor", 
             Subscript["C", name] -> p["C"]["Value"],
          
          "Inductor",  
             Subscript["L", name] -> p["L"]["Value"],
          
          "JosephsonJunction", 
             (* Для JJ используем "C" вместо "CJ" для согласованности *)
             Subscript["C", name] -> p["CJ"]["Value"],
             
          _, {}
        ]
      ]
    ],
    components
  ];
  
  lJeffRules = GetEffectiveInductances[model];
  If[lJeffRules === $Failed, Return[$Failed]];
  
  sMatrixSym = symScattering["SMatrix"];
  sVarSym = symScattering["FrequencyVariable"];
  
  sMatrixNumFunction = sMatrixSym /. Join[valRules, lJeffRules];
  
  omegaList = I * 2 * Pi * frequencyList;
  
  Map[
     Function[w, sMatrixNumFunction /. sVarSym -> w],
     omegaList
  ]
 ];

End[];
EndPackage[];