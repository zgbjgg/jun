# jun
JUN - python pandas support for dataframes manipulation over erlang

[![License: MIT](https://img.shields.io/github/license/zgbjgg/jun.svg)](https://raw.githubusercontent.com/zgbjgg/jun/master/LICENSE)

JUN is a wrapper written in erlang to execute pandas functions and manipuling dataframes creating an isolated environment, so you can handle multiple environments to treat dataframes in each of one them.

This project is under development and should not be used in production, it's not ready for that.

#### Creating an environment

To create an environment you need start a jun worker:

```erlang
(jun@hurakann)1> {ok, Pid} = jun_worker:start_link().
{ok, <0.338.0>}
```

This will create and hold your environment in the `Pid` so you can start with the pandas usage from there.

#### Loading data frames

For now the only way to load a dataframe into jun is reading a csv (in later version we are planning support all methods of pandas api):

```erlang
(jun@hurakann)2> jun_pandas:read_csv(Pid, '/file.csv').
ok
```

Ensure that path exists and is in atom data type. The above code should store the dataframe in pandas datatype into the environment created before, so if you try send a command such a max, min to the environment without a dataframe loaded an error will raise:

```erlang
(jun@hurakann)2> jun_pandas:sum(Pid, 'age'). 
{error,data_frame_is_not_set}
```

Also is posible read the dataframe as the original csv:

```erlang
(jun@hurakann)4> jun_pandas:to_csv(Pid).
{ok,<<",name,age\n0,A,1\n1,B,2\n2,C,3\n3,D,3\n4,R,4\n">>}
```

#### Manipulating dataframe

Now it's time to use some functions over our datafrane previously loaded, for example sum all values of _age_ axis:

```erlang
(jun@hurakann)11> jun_pandas:sum(Pid, age).   
{ok,13}
```

As you can see, the functions is executed through our environment and the response is delivered via the wrapper.

#### Handling errors

All errors will raise as a python errors, describing the class and arguments:

```erlang
(jun@hurakann)3> jun_pandas:sum(Pid, id). 
{error,{'exceptions.KeyError',"Atom('id')"}}
```

#### Function Index

| **Serialization / IO / Conversion** |
|-------------------------------------|
| **[to_csv/1]()** |
| **[read_csv/2]()** |

| **Computations / Descriptive Stats** |
|--------------------------------------|
| **[max/2]()** |
| **[min/2]()** |
| **[count/2]()** |
| **[median/2]()** |
| **[sum/2]()** |

#### Authors

@zgbjgg Jorge Garrido <zgbjgg@gmail.com>
