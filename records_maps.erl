-module(records_maps).
-export([clear_status/1, count_chars/1, count_chars2/1]).
-include_lib("records.hrl").

%% records -> #record_name{}
clear_status(R) when is_record(R, todo) ->
    R#todo{status=finished}.

%% maps -> #{}
%% => create or update KV pair
%% := update existing KV pair

count_chars(Str) ->
    count_chars1(Str, #{}).

%% when H doesnot exists
count_chars1([H|T], X) ->
    %% maps:update_with/4
    %% update H by fun, default value 1, in map X
    count_chars1(T, maps:update_with(H, fun(V) -> V + 1 end, 1, X));
count_chars1([], X) -> X.

count_chars2(Items) ->
    Count = fun(I, A) -> maps:update_with(I, fun(V) -> V + 1 end, 1, A) end,
    lists:foldl(Count, #{}, Items).

