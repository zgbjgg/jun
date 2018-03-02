# Module jun_plotly #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The jun_plotly module provides methods to use plotly integrated into erlang code, similar
to use plot un jun_pandas.

<a name="description"></a>

## Description ##

Example of plotly from erlang:

```erlang

        {ok, Pid} = jun_worker:start_link(),
        {ok, {_, DataFrame}} = jun_pandas:read_csv('/files/csv.txt'),
        {ok, _} = jun_plotly:iplot(Pid, DataFrame, 'line/iplot', [{'kind', 'line'}, {'x', 'name'}, {'y', 'age'}]).
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

### <a name="type-keywords">keywords()</a> ###

<pre><code>
keywords() = [{key :: atom(), value :: any()}, ...]
</code></pre>

## Function Index ##

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index">
  <tr>
    <td valign="top"><a href="#iplot-4">iplot/4</a></td>
    <td>creates a new plot into plotly using a dataframe.</td>
  </tr>
</table>

<a name="functions"></a>

## Function Details ##

<a name="iplot-4"></a>

### iplot/4 ###

<pre><code>
iplot(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, filename :: atom(), <a href="#type-keywords">keywords()</a>) -> {error, no_opaque_dataframe} | {ok, <a href="#type-erlang-dataframe">erlang_dataframe()</a>} 
</code></pre>
<br />

creates a new plot into plotly using a dataframe.
