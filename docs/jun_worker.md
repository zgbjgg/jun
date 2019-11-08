# Module jun_worker #
* [Description](#description)
* [Supported Calls](#supported-calls)

The jun_worker module is responsible for creating a new process holding python environment
into it, so any python command related to pandas, seaborn, numpy, plotly, etc. will be executed
from there. You dont need to know about internal structure if you are only using commands directly
( from jun_pandas for example), however if you want implement a new command just look into this module,
a simple gen_server is used.

<a name="description"></a>

## Description ##

Creates a new jun worker:

```erlang

	{ok, Pid} = jun_worker:start_link()
```

Also is possible to pass the bin as a parameter so you can run the worker with a specific py version:

```erlang

        {ok, Pid} = jun_worker:start_link("python3.6")
```

<a name="supported-calls"></a>

## Supported calls

By default this gen server supports calls to python environment without any module in front, it means
that almost any command not supported in `jun_pandas` or other modules implementing their interface (or shortcut)
to the commands will be used starting from here.

There are various messages to send in the worker in order to work with dataframes, pandas or other tools, please
review the `jun_worker` module, all messages at least has one implementation in the helpers modules such as `jun_pandas`.
