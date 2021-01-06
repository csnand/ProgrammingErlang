-module(records_maps).
-export([clear_status/1, count_chars/1]).
-include_lib("records.hrl").

%% records -> #record_name{}
clear_status(R) when is_record(R, todo) ->
    R#todo{status=finished}.

%% maps -> #{}
%% => create or update KV pair
%% := update existing KV pair

count_chars(Str) ->
    count_chars1(Str, #{}).

%% when H already exists
count_chars1([H|T], #{ H => N }=X) ->
    count_chars1(T, X#{ H := N+1});

%% when H doesnot exists
count_chars1([H|T], X) ->
    count_chars1(T, X#{ H => 1});
count_chars1([], X) -> X.


