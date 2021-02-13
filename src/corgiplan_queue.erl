-module(corgiplan_queue).

-export([new/0, pop/1, insert/3]).

new() ->
    {corgiplan_queue, []}.

insert(Priority, Value, Q) ->
    %% TODO: this is a stub for proper functional heap implementation
    %% other code on the internet does not seem to be tested, or/and uses global state
    %% it is also possible that it would be very difficult to implement in a functional way.
    %% alternatively a different data structure can be used, e.g. an orddict with priorities (i.e. dates)
    %% spaced at a predefiend interval (e.g. 1 minute). I.e. this: https://en.wikipedia.org/wiki/Calendar_queue
    %% we can do 3 buckets. Anything in 1 hour, anything in 1 day, and the rest
    NewData = lists:keysort(1, [{Priority, Value} | Q]),
    {min_heap, NewData}.

pop([{_Priority, Value} | QTail]) ->
    {ok, Value, QTail};
pop([]) ->
    empty.
