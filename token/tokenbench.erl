% tokenbench.erl
%
% Benchmark program to run token.erl a set number of iterations and
% output times to a .dat-file
% Authors: David Henriksson and Eliaz Sundberg

-module(tokenbench).
-import(token, [token/2]).
-import(io, [format/3]).

-export([tokenbench/2]).

tokenbench(Threads, Laps) ->
	{ok, FD} = file:open("./tokenbench_erlang.dat", write),
	tokenbench(2, Laps, Threads, FD).

tokenbench(Threads, _, Threads, _) ->
	{ok, Threads};
tokenbench(Threads, Laps, Iter, FD) ->
	% start timer
	{_, StartSec, StartMicro} = os:timestamp(),

	% run token circle
	token:token(Threads, Laps),

	% end timer
	{_, EndSec, EndMicro} = os:timestamp(),

	Start = (StartSec * 1000000) + StartMicro,
	End = (EndSec * 1000000) + EndMicro,

	% calculate total
	Total = (End - Start) / 1000,

	% write times to ./tokenbench_erlang.dat
	io:format(FD, "~p ~p~n", [Threads, Total]), % pass timer instead of laps

	% repeat until Threads = Iter
	tokenbench(Threads + 1, Laps, Iter, FD).
