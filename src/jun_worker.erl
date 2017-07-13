-module(jun_worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% the values can be override during initialization
-record(state, {py_pid = undefined :: pid(),
    data_frame = undefined :: any()}).

start_link() ->
    % get priv path
    Path = code:priv_dir(jun),
    gen_server:start_link(?MODULE, [Path], []).

init([Path]) ->
    process_flag(trap_exit, true),
    % start the py process and initializes its importing modules
    case python:start([{python_path, Path}]) of
        {ok, PyPid} ->
            lager:info("initialized default modules for py pid ~p", [PyPid]),
            {ok, #state{py_pid = PyPid}};
        Error      ->
            lager:error("cannot initializes py due to ~p", [Error]),
            {stop, Error}
    end.

handle_call({read_csv, Path}, _From, State) ->
    % read csv as data frame pandas and hold in state process if success
    PyPid = State#state.py_pid,
    case catch python:call(PyPid, pandas, read_csv, [Path]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        OpaqueDataFrame                                  ->
            NewState = State#state{data_frame = OpaqueDataFrame},
            {reply, ok, NewState}
    end;
handle_call({_JunFn, _Fn, _Axis}, _From, State = #state{data_frame = undefined}) ->
    {reply, {error, data_frame_is_not_set}, State};
handle_call(to_csv, _From, State) ->
    PyPid = State#state.py_pid,
    DataFrame = State#state.data_frame,
    case catch python:call(PyPid, jun_pandas, df_to_csv, [DataFrame]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        Csv                                              ->
            {reply, {ok, Csv}, State}
    end;
handle_call({JunFn, Fn, Axis}, _From, State) ->
    PyPid = State#state.py_pid,
    DataFrame = State#state.data_frame,
    case catch python:call(PyPid, jun_pandas, JunFn, [DataFrame, Axis, Fn]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        Return                                           ->
            {reply, {ok, Return}, State}
    end;
handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % when finish process just stop py_pid
    ok = python:stop(State#state.py_pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================
