%%
%% This module is a wrapper for sime functions related to pyodbc.
%%
-module(jun_sql_connector).

-export([conn/5]).

%% Common connection

conn(Pid, DSN, Username, Password, Database) ->
    gen_server:call(Pid, {'core.jun.sql', [DSN, Username, Password, Database]}, infinity).
