%%% Round-Trip Latency Benchmark
%%% Course: 7,5 ECTS project
%%% Date: 23/3/12
%%% 
%%% Brian Avlund

-module(rtlb).

-compile([debug_info, export_all]).

receive_send(Next) -> receive
					      {N} -> Next ! {N+1},
                                 receive_send(Next)
					  after 
					      2000 -> io:fwrite("~p DIED ~n", [self()])
    				  end.

main(master) -> P = spawnN(10, self()),
                receiveLoop(1000, 0, P).

spawnN(0, P) -> P;
spawnN(N, P0) -> spawnN(N-1, spawn(fun() -> receive_send(P0) end)).

receiveLoop(0, N, _) -> N;
receiveLoop(I, N, Pid) -> Pid ! {N+1},
                          receive
                              {M} -> receiveLoop(I-1, M, Pid)
                          end.