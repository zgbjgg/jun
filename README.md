# jun

![Jun](https://user-images.githubusercontent.com/1471055/28333993-51d90ad4-6bbf-11e7-98f2-16a9fd2844df.png)

**JUN** - python pandas, plotly, seaborn support & dataframes manipulation over erlang

[![Hex.pm](https://img.shields.io/hexpm/v/jun.svg)](https://hex.pm/packages/jun) 
[![Build Status](https://travis-ci.org/zgbjgg/jun.svg?branch=master)](https://travis-ci.org/zgbjgg/jun)
[![Codecov](https://img.shields.io/codecov/c/github/zgbjgg/jun.svg)](https://codecov.io/gh/zgbjgg/jun)
[![License: MIT](https://img.shields.io/github/license/zgbjgg/jun.svg)](https://raw.githubusercontent.com/zgbjgg/jun/master/LICENSE)

JUN is a wrapper written in erlang to execute pandas, plotly, seaborn functions and manipuling dataframes creating an isolated environment, so you can handle multiple environments to treat dataframes in each of one them.

This project is under development and should not be used in production, it's not ready for that.

### Creating an environment

To create an environment you need start a jun worker:

```erlang
(jun@hurakann)1> {ok, Pid} = jun_worker:start_link().
{ok, <0.338.0>}
```

This will create and hold your environment in the `Pid` so you can start with the pandas usage from there.

### Loading data frames

For now the only way to load a dataframe into jun is reading a csv (in later version we are planning support all methods of pandas api):

```erlang
(jun@hurakann)2> {ok, {_, DataFrame}} = jun_pandas:read_csv(Pid, '/file.csv').
{ok,{'pandas.core.frame.DataFrame', {'$erlport.opaque',python,
                       <<128,2,99,112,97,110,100,97,115,46,99,111,114,101,46,
                         102,114,97,109,101,10,68,97,116,...>>}}}
```

Ensure that path exists and is in atom data type. The above code should return a dataframe in an `opaque` format, so this object is a serializable of the original in py env,
this dataframe is stored into `DataFrame` variable, so you can use for other purposes such execute a query, getting max, min etc.

### Manipulating dataframe

Now it's time to use some functions over our datafrane previously loaded, for example sum all values of _age_ column:

```erlang
(jun@hurakann)3> jun_pandas:sum(Pid, DataFrame, age, []).
{ok,13}
```

As you can see, the functions is executed through our environment and the response is delivered via the wrapper.

### Handling errors

All errors will raise as a python errors, describing the class and arguments:

```erlang
(jun@hurakann)4> jun_pandas:sum(Pid, DataFrame, id, []). 
{error,{'exceptions.KeyError',"Atom('id')"}}
```

### Readable Data Frames into Erlang VM

If you noticed, we are working with `opaque` terms (serialization),, this is because if we design a encoder/decoder for an erlang term to data frame in every execution
this will be a very expensive task, so we decide use opaque terms, however sometimes you need to know the data frame in a readable syntax, for that purpose we design
a single encoder:

```erlang
(jun@hurakann)5> jun_pandas:to_erl(Pid, DataFrame).
{ok,{'pandas.core.frame.DataFrame',[<<"name">>,<<"age">>],
                                   [[<<"A">>,1],
                                    [<<"B">>,2],
                                    [<<"C">>,3],
                                    [<<"D">>,3],
                                    [<<"R">>,4]]}}
```

In the above snippet you can see how is a pandas dataframe represented in erlang term, the first is an atom with the class of the dataframe, the second are
a list with the column names, the third is a list of lists with the values of each row.

#### Modules Index

| **Modules** |
|-------------------------------------|
| **[jun_worker](docs/jun_worker.md)** |
| **[jun_pandas](docs/jun_pandas.md)** |
| **[jun_pandas_series](docs/jun_pandas_series.md)** |
| **[jun_plotly](docs/jun_plotly.md)** |
| **[jun_seaborn]()** |

### See also

[KAA: support to execute jun tasks using protocol buffers](https://github.com/zgbjgg/kaa)

#### Authors

@zgbjgg Jorge Garrido <zgbjgg@gmail.com>
