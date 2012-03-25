%%% Round-Trip Latency Benchmark
%%% Course: 7,5 ECTS project
%%% Date: 23/3/12
%%% 
%%% Brian Avlund

-module(rtlb).

-compile([debug_info, export_all]).

%% ------------------ Flist ------------------ %%
empty() -> fun(_) -> [] end.

add(X, Fl) -> fun(Pre) ->
			  		case Pre of
			  			nothing 			 -> [X | Fl(nothing)];
			  			{just, N} when N > 0 -> [X | Fl({just, N-1})];
						_ 					 -> []
			  		end
			  end.
 
fromList(Xs) -> lists:foldr(fun add/2, empty(), Xs).
			  
toList(Fl) -> Fl(nothing).

merge(Xs, Ys) -> lists:foldr(fun add/2, Ys, toList(Xs)).

fTake(N, Fl) -> Fl({just, N}).

%% ------------------ Simple list ------------------ %%
emptyS() -> [].

addS(X, Fl) -> [X | Fl(nothing)].
 
toList(Fl) -> Fl.


%% ------------------ Round trip latency benchmark ------------------ %%
spawnR(_, 0, P) -> P;
spawnR(Nodes, N, P0) -> P1 = spawn(fun() -> receive_sendR(P0, "this") end),
						spawnR(Nodes, N-1, spawnOnNodesR(Nodes, P1)).

spawnOnNodesR([], Pid) -> Pid;
spawnOnNodesR([N|Nodes], P0) -> P1 = spawn(N, fun() -> receive_sendR(P0, "other") end),
							   	spawnOnNodesR(Nodes, P1).


mainR(master) -> Nodes = [node1@Avlund], % localhost is apparently not working on mac's	
                P = spawnR(Nodes, 10, self()),
                {Time, Res} = timer:tc(fun rtlb:receiveLoopR/3, [1, empty(), P]),
                io:fwrite("Result: ~p in ~p microseconds",[Time, toList(Res)]).

receiveLoopR(0, N, _) -> N;
receiveLoopR(I, N, Pid) -> Pid ! N,
                          receive
                              M -> receiveLoopR(I-1, M, Pid)
                          end.

receive_sendR(Next, Tag) -> receive
					       		N -> Next ! add(Tag, N),
                           		receive_sendR(Next, Tag)
					       after 
					       		2000 -> io:fwrite("~p DIED ~n", [self()])
    				       end.


%% ------------------ Tree structure ------------------ %%
spawnT(0, Self) -> {Self, Self};


spawnT(S, Self) -> L = spawn(fun() -> receive_sendT(spawnT(S-1, Self), "this") end),
			 R = spawn(node1@Avlund, fun() -> receive_sendT(spawnT(S-1, Self), "other") end),
			 {L, R}.


receive_sendT({Lc, Rc}, Tag) -> receive
					       			L0 -> L1 = add(Tag, L0),
					       				  Lc ! L1,
					       				  Rc ! L1,
                           				  receive_sendT({Lc, Rc}, Tag)
					       		after 
					       			2000 -> ok
    				       		end.


mainT(master) -> Layers = 10,
				 P = spawnT(Layers, self()),
				 io:format("~nprocesses: ~p~n", [P]),
                 {Time, Res} = timer:tc(fun rtlb:receiveLoopT/4, [1, add("master", empty()), P, Layers]),
                 % Res = receiveLoopT(1, add("master", empty()), P, Layers),
                 io:fwrite("Result: ~p in ~p microseconds",[Time, length(toList(Res))]).


receiveLoopT(0, List, _, _) -> List;
receiveLoopT(I, List, {Lc, Rc}, Layers) -> Lc ! List,
								           Rc ! List,
			                          	   Ln = gather_list(round(math:pow(2, Layers+1)), List),
			                               receiveLoopT(I-1, Ln, {Lc, Rc}, Layers).


gather_list(0, List) -> List;
gather_list(I, List) -> receive
                            L0 -> gather_list(I-1, merge(L0, List))
    				    end.

