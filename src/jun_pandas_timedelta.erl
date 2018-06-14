%%
%% This module is a wrapper for some functions in pandas for functions and properties
%% of timedelta class.
%%
-module(jun_pandas_timedelta).

-export([days/4]).

%% Attributes

days(Pid, Series, Axis, Keywords) ->
    gen_server:call(Pid, {'core.jun.timedelta', [Series, days, Axis, Keywords]}, infinity).
