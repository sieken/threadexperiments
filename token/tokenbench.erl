-module(tokenbench).
-import(token, [token/2]).
-import(io, [format/3]).

-export([tokenbench/3]).



tokenbench(Threads, Laps, Iter) ->
	{ok, FD} = file:open("./tokenbench_erlang.dat", write),
	tokenbench(Threads, Laps, Iter, FD).

tokenbench(Threads, _, 0, _) ->
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
	Total = (End - Start) / 1000000,

	% write times to ./tokenbench_erlang.dat
	io:format(FD, "~p ~p~n", [Threads, Total]), % pass timer instead of laps

	% repeat until Iter = 0
	tokenbench(Threads + 100, Laps, Iter - 1, FD).
