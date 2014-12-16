%%%%%
%% 
%% Avancerad functonell programmering, HT14
%% Project 3-SAT server, erlang
%% help.erl
%%
%% Jens Rosén
%% 
%% Used by: server.erl
%%
%% Help in tough times.
%% 
%%%%%

-module(help).
-export([is_cnf_head/1,is_cnf_line/1, createList/2, solutionToString/1]).


%% Used to see if a line is a cnf header
%% If it is we return the ok and the variables 
%% and number of clauses. Else no.
is_cnf_head(L) when length(L) > 10 -> 
	case string:tokens(string:substr(L,1,length(L)-2), " ") of
		["p", "cnf", V,C] -> 
			try [list_to_integer(V), list_to_integer(C)] of
				[NV, NC] -> 
					if NV > 0 andalso NC > 0 ->
						   {ok, NV,NC};
					   true -> {no}
					end
			catch
				error:_ -> {no};
				_ -> {no}
			end;
		_ -> {no}
	end;
is_cnf_head(_) ->
	{no}.

%% Used to see if and process 3-SAT clauses.
is_cnf_line(Line) when length(Line) > 2 ->
	NewL = string:tokens(string:substr(Line,1,length(Line)-2), " "),
	Last = lists:last(NewL),
	if Last =:= "0" ->
		try [ list_to_integer(X)|| X<-NewL, X /= "0"] of
			L -> if length(L) < 5 andalso length(L) > 0 ->
					{ok, L};
				true -> {no}
			     end
		catch
			error:_ -> {no};
			ALL -> {ALL}
		end;
	   true -> {no}
	end;
is_cnf_line(_) ->
	{no}.

%% Creates a list from FROM to TO with step size 1
createList(From, To) when From =:= To -> 
	[From];
createList(From, To) when From > To -> 
	[From|createList(From-1,To)];
createList(From, To) ->
	[From|createList(From+1,To)].


%% Used to print the solution in the right format.
solutionToString([]) -> "";
solutionToString([H| Solution]) -> 
	if H -> " t" ++ solutionToString(Solution);
	   true -> " f" ++ solutionToString(Solution)
	end.


