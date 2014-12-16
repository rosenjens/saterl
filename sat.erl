%%%%%
%% 
%% Avancerad functonell programmering, HT14
%% Project 3-SAT server, erlang
%% sat.erl
%%
%% Jens Rosén
%% 
%% Used by: server.erl
%%
%% Does the calculation for the clients connected to
%% the server. This is were the 3-SAT is solved.
%% 
%%%%%

-module(sat).
-export([solve/3, solve1/4]).


%% Called by process that whats to calculate a 3-SAT problem
%% Input: Parent, the parent processor that waits patient for
%%        its child to get a move on.
%%        Lines, the clauses, in the form: [[Clauses 1]..[Clauses N]]
%%        Value, the simple number of varables.
%% Output: (None), all done by message passing, to parent.
%% Can be killed by its parent. That means stop working,
%% you are to slow.
solve(Parent, Lines, Values) ->
	Pid = spawn_link(?MODULE, solve1, [Parent, self(),Lines, Values]),
	messageLoop(Pid, Parent).


%% Simple message loop because we can not loop the solve function
%% se do not want to spawn new children all the time.
messageLoop(Pid, Parent) -> 
	receive
		{kill} -> 	% The parent is tired of us, time to die.
			exit(Pid,socketdied);
		{stop} ->   % The child is tired of us, time to die.
			ok
	after 10000 ->  % The child is taking a lot of time for this best tell the parent.
		      Parent ! {time}, 
		      messageLoop(Pid, Parent)

	end.


%% Time to solve stuff. But first set up the boolean variables.
%% The its time to play.
solve1(Server, Parent, Lines, Values) ->
	Variables = help:createList(1,Values),
	BoolVar = [{true, false}|| _ <- Variables ],
	Msg = solve_(Lines, [], BoolVar),
	case Msg of 
		{solution, M} -> Server ! {solution, M};
		{no} -> Server ! {unsolved}
	end,
	Parent ! {stop}.


%% Guessing the solution to the problem. Take the two boolean
%% variables and split into two solutions. When the domains are
%% set we check if the guess is a solution. If not we take the other
%% branch.
solve_(Lines, Guess, []) -> 
	check(Lines, Guess);
solve_(Lines, Guess,  [{A,B}|BoolVar]) -> 
	%io:format("Guess: ~p ~n",[Guess]),
	Right = solve_(Lines, Guess++[A], BoolVar), % go right first
	case Right of 
		{solution, S} -> 
			{solution, S};
		{no} -> 
			Left = solve_(Lines, Guess++[B], BoolVar), % go left if right was bad
			case Left of
				{solution, S} -> 
					{solution, S};
				{no} -> {no}
			end
	end.


%% Check if the gessed values resulted in a valid solution
check([], Guess) -> {solution, Guess};
check([A|Lines], Guess) ->
	Test = check_(A,Guess),
	case Test of
		{ok} -> 
			check(Lines, Guess);
		{no} -> {no}
	end.

%% Test if a Clauses is true.
%% That is if one of the variables in the Clauses
%% correspons to true.
check_([], _) -> 
	{no};
check_([H|T], Guess) ->
	Value = getValue(H, Guess),
	if Value -> 
		   {ok};
	   true -> 
		   check_(T, Guess)
	end. 



%% Look up value of position H in the guessed values.
getValue(H, Guess) ->
	if H < 0 ->
		   op(lists:nth(abs(H), Guess));
	   true ->
		   lists:nth(H, Guess)
	end.



%% negate the values
op(false)-> 
	true;
op(true) -> 
	false.




	


