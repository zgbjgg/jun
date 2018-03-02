# Module jun_pandas #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The jun_pandas module implements the methods to a pandas python class using a very easy
erlang syntax.

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

### <a name="type-erlang-dataframe">dataframe()</a> ###

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

<a name="index"></a>

## Function Index ##

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index">
  <tr>
    <td valign="top"><a href="#to_erl-2">to_erl/2</a></td>
    <td>returns an opaque dataframe in erlang readable terms.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#max-4">max/4</a></td>
    <td>calculates the max value of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#min-4">max/4</a></td>
    <td>calculates the min value of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#count-4">max/4</a></td>
    <td>calculates the count of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#median-4">max/4</a></td>
    <td>calculates the median value of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#sum-4">max/4</a></td>
    <td>calculates the sum value of a column or in an entire grouped dataframe.</td>
  </tr>
</table>

<a name="functions"></a>

## Function Details ##

<a name="to_erl-2"></a>

### to_erl/2 ###

<pre><code>
to_erl(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>) -> {error, no_opaque_dataframe} | {ok, <a href="#type-erlang-dataframe">erlang_dataframe()</a>} 
</code></pre>
<br />

returns an opaque dataframe in erlang readable terms.

<a name="max-4"></a>

### max/4 ###

<pre><code>
max(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, atom(), <a href="#type-keywords"></a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the max value of a column or in an entire grouped dataframe.

<a name="min-4"></a>

### min/4 ###

<pre><code>
min(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, atom(), <a href="#type-keywords"></a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the min value of a column or in an entire grouped dataframe.

<a name="count-4"></a>

### count/4 ###

<pre><code>
count(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, atom(), <a href="#type-keywords"></a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the count of a column or in an entire grouped dataframe.

<a name="median-4"></a>

### median/4 ###

<pre><code>
median(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, atom(), <a href="#type-keywords"></a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the median value of a column or in an entire grouped dataframe.

<a name="sum-4"></a>

### sum/4 ###

<pre><code>
sum(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, atom(), <a href="#type-keywords"></a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the sum value of a column or in an entire grouped dataframe.

