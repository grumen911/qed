BeginPackage["QED`CircuitTopology`"]

(* Публичный конструктор *)
CreateTopology::usage = "CreateTopology[components, groundNode] создаёт расширенную 
						топологическую структуру для Model.wl";

Begin["`Private`"] (* Begin Private Context *) 

CreateTopology[components_List, groundNode_Integer] := 
 Module[{nodes, edges, capacitorEdges, spanningTree, nodePaths, graph,
 		fluxLoops, inductiveEdges, inductiveGraph, inductiveTreeGraph,
 		treeTags, realInductiveChords},
  
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
  
  (* 4.5. Robust Identification of Superconducting Loops *)
  
  (* 1. Выделяем все индуктивные ребра с именами (тегами) *)
  inductiveEdges = Cases[components, 
    {"JosephsonJunction" | "Inductor", n1_, n2_, name_} :> 
    UndirectedEdge[n1, n2, name]
  ];
  
  (* 2. Если индуктивностей нет или мало, возвращаем пусто *)
  If[inductiveEdges === {}, 
     fluxLoops = <||>,
     
     (* 3. Строим индуктивный граф и его остовное дерево (Лес) *)
     inductiveGraph = Graph[nodes, inductiveEdges];
     inductiveTreeGraph = FindSpanningTree[inductiveGraph];
     
     (* Список имен (тегов) компонентов, попавших в дерево *)
     treeTags = EdgeList[inductiveTreeGraph][[All, 3]];
     
     (* 4. Хорды = Индуктивности, не попавшие в дерево *)
     (* Эти элементы однозначно замыкают фундаментальные циклы *)
     realInductiveChords = Select[components,
        MatchQ[#[[1]], "JosephsonJunction" | "Inductor"] &&
        !MemberQ[treeTags, #[[4]]] &
     ];
     
     (* 5. Строим циклы для каждой хорды *)
     fluxLoops = Association @ Map[
        Function[chord,
           (* Ищем путь в дереве между концами хорды *)
           With[{pathNodes = FindShortestPath[inductiveTreeGraph, chord[[2]], chord[[3]]]},
             
             (* Восстанавливаем имена компонентов пути *)
             Module[{loopComponents, u, v, edge},
                loopComponents = {chord[[4]]}; (* Цикл начинается с хорды *)
                
                Do[
                  u = pathNodes[[i]]; v = pathNodes[[i+1]];
                  (* Ищем ребро дерева между u и v *)
                  (* EdgeList возвращает {UndirectedEdge[n1, n2, name]...} *)
                  (* Нам нужно найти то, где (n1=u, n2=v) или (n1=v, n2=u) *)
                  edge = SelectFirst[EdgeList[inductiveTreeGraph], 
                     ( #[[1]]==u && #[[2]]==v ) || ( #[[1]]==v && #[[2]]==u ) &
                  ];
                  AppendTo[loopComponents, edge[[3]]];
                , {i, Length[pathNodes]-1}];
                
                (* Возвращаем структуру петли. Ключ = имя хорды (уникально) *)
                chord[[4]] -> <|
                  "ChordElement" -> chord[[4]],
                  "LoopComponents" -> loopComponents,
                  "ExternalFluxVariable" -> "Fext_" <> chord[[4]]
                |>
             ]
           ]
        ],
        realInductiveChords
     ];
  ];

  (* 5. Возвращаем структуру, готовую для Primary Parameters *)
  <|
    "Type" -> "SuperconductingCircuit",
    "Nodes" -> nodes,
    "GroundNode" -> groundNode,
    "Components" -> components,
    "GraphStructure" -> <|
       "FullGraph" ->  Graph[nodes, edges, EdgeLabels -> Automatic],
       "SpanningTree" -> spanningTree,
       "FluxPaths" -> nodePaths,
       "fluxLoops" -> fluxLoops
    |>,
    "DegreesOfFreedom" -> Length[nodes] - 1 (* количество активных переменных *)
  |>
 ]

End[];
EndPackage[];