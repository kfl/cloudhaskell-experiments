%%% Round-Trip Latency Benchmark
%%% Course: 7,5 ECTS project
%%% Date: 30/3/12
%%% 
%%% Brian Avlund

-module(rtlb).

-compile([debug_info, export_all]).


%% ------------------ Round trip latency benchmark ------------------ %%
spawnR(_, 0, P) -> P;
spawnR(Nodes, N, P0) -> P1 = spawn(fun() -> receive_sendR(P0, "this") end),
						spawnR(Nodes, N-1, spawnOnNodesR(Nodes, P1)).

spawnOnNodesR([], Pid) -> Pid;
spawnOnNodesR([N|_], P0) -> spawn(N, fun() -> receive_sendR(P0, "other") end).


% main() -> Nodes = [], % localhost is apparently not working on mac's	
main() -> Nodes = [node1@Avlund], % localhost is apparently not working on mac's	
          P = spawnR(Nodes, 50, self()),
          {Time, Res} = timer:tc(fun rtlb:receiveLoopR/3, [1, structs:emptyS(), P]),
          io:fwrite("Result: ~p in ~p microseconds",[length(structs:toListS(Res)), Time]).

receiveLoopR(0, N, _) -> N;
receiveLoopR(I, N, Pid) -> Pid ! N,
                          receive
                              M -> receiveLoopR(I-1, M, Pid)
                          end.

receive_sendR(Next, Tag) -> receive
					       		N -> Next ! structs:addS(Tag, N)
    				       end.