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
			  			nothing -> [X | Fl(nothing)];
			  			{just, N} when N > 0 -> [X | Fl({just, N-1})];
						_ -> []
			  		end
			  end.
 
fromList(Xs) -> lists:foldr(fun add/2, empty(), Xs).
			  
toList(Fl) -> Fl(nothing).

fTake(N, Fl) -> Fl({just, N}).


%% ------------------ Round trip latency benchmark ------------------ %%
receive_send(Next, Tag) -> receive
					       		N -> Next ! add(Tag, N),
                           		receive_send(Next, Tag)
					       after 
					       		2000 -> io:fwrite("~p DIED ~n", [self()])
    				       end.

spawnN(_, 0, P) -> P;
spawnN(Nodes, N, P0) -> P1 = spawn(fun() -> receive_send(P0, "this") end),
						spawnN(Nodes, N-1, spawnOnNodes(Nodes, P1)).
						
spawnOnNodes([], Pid) -> Pid;
spawnOnNodes([N|Nodes], P0) -> P1 = spawn(N, fun() -> receive_send(P0, "other") end),
							   		spawnOnNodes(Nodes, P1).
					  
main(master) -> Nodes = [node1@Avlund], % localhost is apparently working on mac's
io:format("~p", [Nodes]),	
                P = spawnN(Nodes, 10, self()),
                {Time, Res} = timer:tc(fun rtlb:receiveLoop/3, [1, empty(), P]),
                io:fwrite("Result: ~p in ~p microseconds",[Time, toList(Res)]).

receiveLoop(0, N, _) -> N;
receiveLoop(I, N, Pid) -> Pid ! N,
                          receive
                              M -> receiveLoop(I-1, M, Pid)
                          end.

