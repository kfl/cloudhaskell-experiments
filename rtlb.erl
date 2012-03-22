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
			  			{nothing} -> [X | Fl({nothing})];
			  			{just, N} when N > 0 -> [X | Fl({just, N-1})];
						_ -> []
			  		end
			  end.
 
fromList(Xs) -> lists:foldr(fun add/2, empty(), Xs).
			  
toList(Fl) -> Fl({nothing}).

fTake(N, Fl) -> Fl({just, N}).


%% ------------------ Round trip latency benchmark ------------------ %%
receive_send(Next) -> receive
					      {N} -> Next ! {N+1},
                                 receive_send(Next)
					  after 
					      2000 -> io:fwrite("~p DIED ~n", [self()])
    				  end.

main(master) -> P = spawnN(10, self()),
                {Time, Res} = timer:tc(fun rtlb:receiveLoop/3, [1000, 0, P]),
                io:fwrite("Result: ~p in ~p microseconds",[Time, Res]).

spawnN(0, P) -> P;
spawnN(N, P0) -> spawnN(N-1, spawn(fun() -> receive_send(P0) end)).

receiveLoop(0, N, _) -> N;
receiveLoop(I, N, Pid) -> Pid ! {N+1},
                          receive
                              {M} -> receiveLoop(I-1, M, Pid)
                          end.

