% token.erl
%
% Creates a circular structure of processes and passes a token
% between them for a set number of laps
% Used by tokenbench.erl
% Authors: David Henriksson and Eliaz Sundberg

-module(token).
-import(lists, [last/1]).

-export([token/2, thread_func/1]).

token(NThreads, NLaps) ->
	create_and_spawn(start, NThreads, NLaps),
	ok.

% Handler process is a part of the circular structure
% Manages counter
handler(List, 1) ->
	[Next|_] = List,
	Next ! stop,
	receive
		stop ->
			io:fwrite("Handler received stop message, ending~n", []),
			ok
	end;
handler(List, LapCounter) ->
	[Next|_] = List,
	receive
		token ->
			Next ! token,
			handler(List, LapCounter - 1)
	end.

% Creates and spawns NThreads
create_and_spawn(start, NThreads, NLaps) ->
	[H|T] = create_and_spawn([], NThreads),
	H ! token,
	handler([H|T], NLaps - 1).
create_and_spawn(List, 0) ->
	[H|_] = List,
	Pid = spawn(token, thread_func, [H]),
	[Pid|List];
create_and_spawn([], Counter) ->
	Pid = spawn(token, thread_func, [self()]),
	create_and_spawn([Pid], Counter - 1);
create_and_spawn(List, Counter) ->
	[H|_] = List,
	Pid = spawn(token, thread_func, [H]),
	create_and_spawn([Pid|List], Counter - 1).

% Thread procedure, waits for message and responds accordingly
thread_func(Pid) ->
	receive
		token ->
			Pid ! token,
			thread_func(Pid);
		stop ->
			Pid ! stop,
			{ok, self()}
	end.
