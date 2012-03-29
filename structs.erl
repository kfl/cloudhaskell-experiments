-module(structs).

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

addS(X, Fl) -> [X | Fl].
 
toListS(Fl) -> Fl.

mergeS(Xs, Ys) -> lists:foldr(fun addS/2, Ys, Xs).