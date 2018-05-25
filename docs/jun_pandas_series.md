# Module jun_pandas_series #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The jun_pandas_series module implements the methods to a pandas python class specific for series 
using a very easy erlang syntax.

<a name="description"></a>

## Description ##

Example of reading a csv:

```erlang

        {ok, Pid} = jun_worker:start_link(),
        {ok, Dataframe} = jun_pandas:read_csv('/file/csv.txt').
```

<a name="types"></a>

## Data Types ##

### <a name="type-jun-worker">jun_worker()</a> ###

<pre><code>
jun_worker() = pid() 
</code></pre>

### <a name="type-jun-dataframe">jun_dataframe()</a> ###

<pre><code>
jun_dataframe() = {'pandas.core.frame.DataFrame', <a href="#type-dataframe">dataframe()</a>}
</code></pre>

### <a name="type-dataframe">dataframe()</a> ###

<pre><code>
dataframe() = {'$erlport.opaque', python, binary()}
</code></pre>

### <a name="type-erlang-dataframe">erlang_dataframe()</a> ###

<pre><code>
erlang_dataframe() = {'pandas.core.frame.DataFrame', [column :: binary(), ...], [[value :: any(), ...]]}
</code></pre>

### <a name="type-keywords">keywords()</a> ###

<pre><code>
keywords() = [{key :: atom(), value :: any()}, ...]
</code></pre>

### <a name="type-jun-error">jun_error()</a> ###

<pre><code>
jun_error() = {error, {exception :: atom(), description :: string()}
</code></pre>

### <a name="type-jun-subplot">jun_subplot()</a> ###

<pre><code>
jun_subplot() = <<"matplotlib.AxesSubplot">>
</code></pre>

### <a name="type-jun-groupby">jun_groupby()</a> ###

<pre><code>
jun_groupby() = {'pandas.core.groupby.DataFrameGroupBy', <a href="#type-groupby">groupby()</a>}
</code></pre>

### <a name="type-groupby">groupby()</a> ###

<pre><code>
groupby() = {'$erlport.opaque', python, binary()}
</code></pre>

### <a name="type-jun-series">jun_series()</a> ###

<pre><code>
jun_series() = {'pandas.core.frame.Series', <a href="#type-series">series()</a>}
</code></pre>

### <a name="type-series">series()</a> ###

<pre><code>
series() = {'$erlport.opaque', python, binary()}
</code></pre>

<a name="index"></a>

## Function Index ##

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index">
  <tr>
    <td valign="top"><a href="#combine-4">to_erl/2</a></td>
    <td>combine two series of a dataframe in one.</td>
  </tr>
</table>

<a name="functions"></a>

## Function Details ##

<a name="combine-4"></a>

### combine/4 ###

<pre><code>
combine(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-series">series()</a>, <a href="#type-series">series()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, series :: jun_series()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>
<br />

returns an opaque dataframe in erlang readable terms.
