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
        {ok, {_, DataFrame}} = jun_pandas:read_csv(Pid, <<"/files/csv.txt">>, []),
        {ok, _} = jun_plotly:iplot(Pid, DataFrame, <<"line/iplot">>, [{<<"kind">>, <<"line">>}, {<<"x">>, <<"name">>}, {<<"y">>, <<"age">>}]).
```

<a name="types"></a>

## Data Types ##

### <a name="type-jun-worker">jun_worker()</a> ###

<pre><code>
jun_worker() = pid() 
</code></pre>

### <a name="type-jun-dataframe">jun_dataframe()</a> ###

<pre><code>
jun_dataframe() = {<<"pandas.core.frame.DataFrame">>, <a href="#type-dataframe">dataframe()</a>}
</code></pre>

### <a name="type-dataframe">dataframe()</a> ###

<pre><code>
dataframe() = {'$erlport.opaque', python, binary()}
</code></pre>

### <a name="type-keywords">keywords()</a> ###

<pre><code>
keywords() = [{key :: binary(), value :: any()}, ...]
</code></pre>

### <a name="type-plotly">plotly()</a> ###

<pre><code>
plotly() = {<<"plotly.iplot">>, binary()}
</code></pre>

## Function Index ##

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index">
  <tr>
    <td valign="top"><a href="#iplot-4">iplot/4</a></td>
    <td>creates a new plot into plotly using a dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#plot-4">plot/4</a></td>
    <td>creates a new plot into a valid plotly url.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#extend-4">extend/4</a></td>
    <td>extends two plots into one (like merge them).</td>
  </tr>
  <tr>
    <td valign="top"><a href="#get-figure-4">get_figure/4</a></td>
    <td>gets the figure (data representation) from a valid plotly url.</td>
  </tr>
</table>

<a name="functions"></a>

## Function Details ##

<a name="iplot-4"></a>

### iplot/4 ###

<pre><code>
iplot(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, filename :: binary(), <a href="#type-keywords">keywords()</a>) -> {error, no_opaque_dataframe} | {ok, <a href="#type-plotly">plotly()</a>} 
</code></pre>
<br />

creates a new plot into plotly using a dataframe.

<a name="plot-4"></a>

### plot/4 ###

<pre><code>
plot(<a href="#type-jun-worker">jun_worker()</a>, iplot :: binary(), filename :: binary(), <a href="#type-keywords">keywords()</a>) -> {ok, url :: string()}
</code></pre>

creates a new plot into a valid plotly url.

<a name="extend-4"></a>

### extend/4 ###

<pre><code>
extend(<a href="#type-jun-worker">jun_worker()</a>, iplotx :: binary(), iploty :: binary(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="#type-plotly">plotly()</a>} 
</code></pre>

extends two plots into one (like merge them).

<a name="get-figure-4"></a>

### get_figure/4 ###

<pre><code>
get_figure(<a href="#type-jun-worker">jun_worker()</a>, url :: binary(), filename :: binary(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="#type-plotly">plotly()</a>}
</code></pre>

get the figure (data representation) from a valid plotly url.
