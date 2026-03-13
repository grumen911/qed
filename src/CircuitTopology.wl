BeginPackage["QED`CircuitTopology`"];

CreateTopology::usage = "CreateTopology[components, groundNode] builds the complete topological \
structure of a superconducting circuit from a netlist.

Arguments:
  components: A list of circuit elements, e.g., {{\"Capacitor\", 1, 2, \"C1\"}, ...}.
  groundNode: Integer representing the reference ground node.

Under the hood, this function performs:
  1. Node and edge extraction to build the full circuit graph.
  2. Capacitive spanning tree generation for defining flux paths.
  3. DC-connectivity analysis to identify grounded subgraphs and isolated floating islands \
     (which mathematically correspond to global zero modes).
  4. Inductive spanning forest and chord analysis to rigorously identify fundamental flux loops.

Returns an Association containing the topological metadata, including \"GraphStructure\" \
(with \"fluxLoops\", \"FloatingIslands\", and \"ZeroModeNodes\") and the effective \
\"DegreesOfFreedom\" used for Hamiltonian reduction.";


Begin["`Private`"];


CreateTopology[components_List, groundNode_Integer] := 
 Module[{nodes, edges, capacitorEdges, spanningTree, nodePaths, graph,
      fluxLoops, inductiveEdges, inductiveGraph, inductiveTreeGraph,
      treeTags, realInductiveChords, connectedComps, groundedNodes, floatingIslands},
  
  (* 1. Извлекаем все уникальные узлы и ребра*)
  nodes = Union[Flatten[{components[[All, 2]], components[[All, 3]]}]];
  edges = UndirectedEdge[#[[2]], #[[3]], #[[4]]] & /@ components;
  
  (* 2. Строим граф только из ёмкостей для поиска остовного дерева *)
  capacitorEdges = Cases[components, {"Capacitor", n1_, n2_, name_} |
                             {"JosephsonJunction", n1_, n2_, name_} :>
                             UndirectedEdge[n1, n2, name]];
  
  (* 3. Находим остовное дерево (Spanning Tree) с корнем в groundNode *)
  graph = Graph[nodes, capacitorEdges];
  spanningTree = FindSpanningTree[graph, Root -> groundNode];
  
  (* 4. Генерируем пути от земли до каждого узла *)
  nodePaths = AssociationMap[
    FindShortestPath[spanningTree, groundNode, #] &, 
    nodes
  ];
  
  (* 4.5. Robust Identification of Superconducting Loops and Islands *)
  
  (* 1. Выделяем все индуктивные ребра с именами (тегами) *)
  inductiveEdges = Cases[components, 
    {"JosephsonJunction" | "Inductor", n1_, n2_, name_} :> 
    UndirectedEdge[n1, n2, name]
  ];
  
  (* --- ИСПРАВЛЕНИЕ: Анализ островов делаем ДО If --- *)
  (* Строим индуктивный граф (если ребер нет, это будет просто набор изолированных узлов) *)
  inductiveGraph = Graph[nodes, inductiveEdges];

  (* Находим все компоненты связности по постоянному току *)
  connectedComps = ConnectedComponents[inductiveGraph];
     
  (* Компонента, содержащая землю - это "заземленные" узлы *)
  groundedNodes = First @ Select[connectedComps, MemberQ[#, groundNode] &];
     
  (* Все остальные компоненты - это изолированные острова (каждый дает 1 нулевую моду) *)
  floatingIslands = Select[connectedComps, !MemberQ[#, groundNode] &];
  (* --------------------------------------------------- *)

  (* 2. Обработка петель *)
  If[inductiveEdges === {}, 
     fluxLoops = <||>,
     
     (* 3. Строим остовное дерево (Лес) для индуктивного графа *)
     inductiveTreeGraph = FindSpanningTree[inductiveGraph];
     
     (* Список имен (тегов) компонентов, попавших в дерево *)
     treeTags = EdgeList[inductiveTreeGraph][[All, 3]];
     
     (* 4. Хорды = Индуктивности, не попавшие в дерево *)
     realInductiveChords = Select[components,
        MatchQ[#[[1]], "JosephsonJunction" | "Inductor"] &&
        !MemberQ[treeTags, #[[4]]] &
     ];
     
     (* 5. Строим циклы для каждой хорды *)
     fluxLoops = Association @ Map[
        Function[chord,
           With[{pathNodes = FindShortestPath[inductiveTreeGraph, chord[[2]], chord[[3]]]},
             Module[{loopComponents, u, v, edge},
                loopComponents = {chord[[4]]}; 
                
                Do[
                  u = pathNodes[[i]]; v = pathNodes[[i+1]];
                  edge = SelectFirst[EdgeList[inductiveTreeGraph], 
                      ( #[[1]]==u && #[[2]]==v ) || ( #[[1]]==v && #[[2]]==u ) &
                  ];
                  AppendTo[loopComponents, edge[[3]]];
                , {i, Length[pathNodes]-1}];
                
                chord[[4]] -> <|
                  "ChordElement" -> chord[[4]],
                  "LoopComponents" -> loopComponents,
                  "ExternalFluxVariable" -> "Fext",
                  "ExternalFluxSymbol" -> QED`$PhiExt
                |>
             ]
           ]
        ],
        realInductiveChords
     ];
  ];

  (* 5. Возвращаем структуру *)
  <|
    "Type" -> "SuperconductingCircuit",
    "Nodes" -> nodes,
    "GroundNode" -> groundNode,
    "Components" -> components,
    "GraphStructure" -> <|
       "FullGraph" ->  Graph[nodes, edges, EdgeLabels -> Automatic],
       "SpanningTree" -> spanningTree,
       "FluxPaths" -> nodePaths,
       "fluxLoops" -> fluxLoops,
       (* Упаковываем данные о нулевых модах прямо в GraphStructure *)
       "FloatingIslands" -> floatingIslands,
       "ZeroModeNodes" -> Map[First, floatingIslands] (* Берем первые узлы островов как "якоря" *)
    |>,
    "DegreesOfFreedom" -> Length[nodes] - 1 - Length[floatingIslands]
  |>
 ]

End[];
EndPackage[];