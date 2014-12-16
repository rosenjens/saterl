%%%%%
%%
%% Avancerad functonell programmering, HT14
%% Project 3-SAT server, erlang
%% server.erl
%%
%% Jens Rosén
%%
%% Uses help.erl, sat.erl
%%
%% Start with run for a server in a new process.
%% Start with start for a server in the same process.
%%
%% Starts a server on post PORT.
%%
%%
%% A server example usage:
%% An example of a boolean expression in 3 variables and 2 clauses:
%% > ready
%% > p cnf 3 2
%% > 1 -3 0
%% > 2 3 -1 0
%% > sat t t t
%%
%% corresponds to:
%% (X1 OR (NOT X3)) AND (X2 OR X3 OR (NOT X1))
%% gave the answer: X1 = T, X2 = T, X3 = T
%%
%%%%%

-module(server).
-export([run/0,start/0,server/2]).

-define(PORT, 3547).
-define(WORKERS, 20).
-define(MAX, 8).
-define(TCP_O,  [list, {packet, line}, {active, false}, {reuseaddr, true}]).


%% Start the server in another process
run() ->
	spawn_link(?MODULE, start, []).

%% Starts the server in the same process
start() ->
	io:format("Start server...~n",[]),
	case gen_tcp:listen(?PORT, ?TCP_O) of
		{ok, LS} ->
			start_server(?WORKERS, LS);
		NOTok ->
			io:format("Something when wrong, bad stuff: ~p.~n", [NOTok])
	end.

%% Start the workers that should listen for tcp connections. 
start_server(0, _) -> 
	io:format("~p workers started.~n",[?WORKERS]),
	count(0);
start_server(N, LS) ->
	spawn_link(?MODULE, server , [LS, self()]),
	start_server(N-1, LS).


%% The server should one handel a specified number (?MAX) of
%% connections at a given time. Count keeps track of the 
%% number of connected clients. 
count(N) ->
	receive
		{is_ok, PID} ->
			if N < ?MAX ->
				  PID ! {ok},
				  count(N+1);
			   true ->
				   PID ! {ook}
			end;
		{finished} ->
			count(N-1)
	end,
	count(N).
	

%% The start of the worker process. Waiting of a connection
%% from a client. 
server(LS, Server) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            Server ! {is_ok, self()}, % Asks if num clients < MAX.
	        receive
		        {ok} -> 
			        sendMessage(S, "ready\n"), 
			        loop(S, Server);
		        {ook} -> 
			        sendMessage(S, "busy\n"),
			        gen_tcp:close(S)
	        end;
        NOTok ->
			io:format("Something when wrong, bad stuff: ~p.~n", [NOTok])
    end,
    server(LS, Server).



%% The server loop before receving the cnf header.
loop(S, Server) ->
   inet:setopts(S,[{active,once}]), %data from socket will be sent to the process message.
    receive
        {tcp,S,Data} ->
		    case help:is_cnf_head(Data) of %% Check the input data from the client.
			    {ok, V, C} -> 
				    Lines = readLines(S,Server, C, []),
				    case Lines of
					    {no} -> ok;
					    _ -> Pid = spawn_link(sat, solve , [self(), Lines, V]),
                             sendMessage(S, "trying\n"),
                             solvingLoop(Pid, S, Server)
				    end;
				_ -> 
				    sendMessage(S,"ignored\n"),
				    sendMessage(S, "ready\n"),
				    loop(S, Server)
		    end;
        {tcp_closed,S} ->
            	Server ! {finished}, % Reduce the count on active connections
                closeMessage(S, self()),
		ok
    after 60000 ->			% After one min, timeout, close connection
		sendMessage(S, "timeout\n"),
		gen_tcp:close(S),
		Server ! {finished},
		ok
    end.


%% The loop for when the process has started to solve a 3-SAT.
solvingLoop(Pid, S, Server) ->
	inet:setopts(S,[{active,once}]),
	receive
		{tcp,S,Data} -> 
            % If the user whats to abort, we kill it!
			if Data =:= "abort\r\n" ->
                   Pid ! {kill},
				   sendMessage(S, "aborted\n"),
				   sendMessage(S, "ready\n"),
				   loop(S, Server);
               % All other messages are seen as false ones and ignored
			   true ->
				sendMessage(S,"ignored\n"),
				solvingLoop(Pid, S, Server)
			end;
		{tcp_closed, S} ->
            Pid ! {kill},
			Server ! {finished},
			closeMessage(self(),S),
            ok;
		{solution, Solution} ->
			Svar = "sat" ++ help:solutionToString(Solution) ++ "\n",
			sendMessage(S, Svar),
			sendMessage(S, "ready\n"),
			loop(S, Server);
		{unsolved} ->
			sendMessage(S, "unsat\n"),
			sendMessage(S, "ready\n"),
			loop(S, Server);
        {aborted} ->
            sendMessage(S, "aborted\n"),
			sendMessage(S, "ready\n"),
			loop(S, Server);
        {time} ->
            sendMessage(S, "trying\n"),
		    solvingLoop(Pid, S, Server)
	end.

%% Read the 3-SAT lines. All N of them.
%% Returns in the form of [[Clauses 1]..[Clauses N]]
readLines(_,_, N, A) when N =< 0 ->
	A;
readLines(S,Server, N, A) ->
	inet:setopts(S,[{active,once}]),
	receive
		{tcp,S,Data} -> 
			case help:is_cnf_line(Data) of 
				{ok, L} ->
					readLines(S, Server, N-1, A++[L]);
				{no} ->
					sendMessage(S,"ignored\n"),
					sendMessage(S,"try again...\n"),
					readLines(S, Server, N, A)
					

			end;
		{tcp_closed,S} -> 
			Server ! {finished},
			closeMessage(self(), S),
            {no}

	end.

%% Help function for sending messages to the client
sendMessage(ToSock, MSG) ->
	gen_tcp:send(ToSock, MSG).

%% Help function, prints a closing message on the server.
closeMessage(Self, S) ->
    io:format("<~p> Socket ~p has closed!~n",[Self, S]).

