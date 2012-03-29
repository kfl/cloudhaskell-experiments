%%% Round-Trip Latency Benchmark
%%% Course: 7,5 ECTS project
%%% Date: 30/3/12
%%% 
%%% Brian Avlund

-module(tree).

-compile([debug_info, export_all]).

%% ------------------ Tree structure ------------------ %%
spawnT(0, Self) -> {Self, Self};

spawnT(S, Self) -> L = spawn(fun() -> receive_sendT(spawnT(S-1, Self), "this") end),
			 	   R = spawn(node1@Avlund, fun() -> receive_sendT(spawnT(S-1, Self), "other") end),
			       {L, R}.


receive_sendT({Lc, Rc}, Tag) -> receive
					       			L0 -> L1 = structs:add(Tag, L0),
					       				  Lc ! L1,
					       				  Rc ! L1,
                           				  receive_sendT({Lc, Rc}, Tag)
					       		after 
					       			2000 -> ok
    				       		end.


main(master) -> Layers = 4,
				P = spawnT(Layers, self()),
				io:format("~nprocesses: ~p~n", [P]),
                {Time, Res} = timer:tc(fun rtlb:receiveLoopT/4, [1, structs:add("master", structs:empty()), P, Layers]),
                io:fwrite("Result: ~p in ~p microseconds",[Time, length(structs:toList(Res))]).


receiveLoopT(0, List, _, _) -> List;
receiveLoopT(I, List, {Lc, Rc}, Layers) -> Lc ! List,
								           Rc ! List,
			                          	   Ln = gather_list(round(math:pow(2, Layers+1)), List),
			                               receiveLoopT(I-1, Ln, {Lc, Rc}, Layers).


gather_list(0, List) -> List;
gather_list(I, List) -> receive
                            L0 -> gather_list(I-1, structs:merge(L0, List))
    				    end.