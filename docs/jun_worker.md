# Module jun_worker #
* [Description] (#description)
* [Data Types] (#types)
* [Function Index] (#index)
* [Function Details] (#functions)

The jun_worker module is responsible for creating a new process holding python environment
into it, so any python command related to pandas, seaborn, numpy, plotly, etc. will be executed
from there. You dont know about internal structure if you are only using commands directly, however
if you want implement a new command just look into this module, a simple gen_server is used.

<a name="description"></a>

## Description ##

Creates a new jun worker:

```erlang

	{ok, Pid} = jun_worker:start_link()
```
