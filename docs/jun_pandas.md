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
    <td valign="top"><a href="#to_erl-2">to_erl/2</a></td>
    <td>returns an opaque dataframe in erlang readable terms.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#max-4">max/4</a></td>
    <td>calculates the max value of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#min-4">min/4</a></td>
    <td>calculates the min value of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#count-4">count/4</a></td>
    <td>calculates the count of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#median-4">median/4</a></td>
    <td>calculates the median value of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#sum-4">sum/4</a></td>
    <td>calculates the sum value of a column or in an entire grouped dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#unique-4">unique/4</a></td>
    <td>return unique values in a column of dataframe.</td>
  </tr>  
  <tr>
    <td valign="top"><a href="#read-csv-2">read_csv/2</a></td>
    <td>read a csv and transforms into a valid dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#to-csv-3">to_csv/3</a></td>
    <td>transforms a dataframe into a csv format.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#to-html-3">to_html/3</a></td>
    <td>transforms a dataframe into an html format.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#to-json-3">to_json/3</a></td>
    <td>transforms a dataframe into a json format.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#to-datetime-3">to_datetime/3</a></td>
    <td>transforms a column of dataframe into a valid datetime dtype.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#read-sql-2">read_sql/2</a></td>
    <td>read a sql query and transforms into a valid dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#query-4">query/4</a></td>
    <td>execute a valid query into dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#first-4">first/4</a></td>
    <td>gets the N first elements.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#last-4">last/4</a></td>
    <td>gets the N last elements.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#columns-3">columns/3</a></td>
    <td>gets the column names as binary separated by comma in dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#len-columns-4">len_columns/3</a></td>
    <td>gets the total number of columns in dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#len-index-3">len_index/3</a></td>
    <td>gets the length of index in a dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#memory-usage-3">memory_usage/3</a></td>
    <td>gets the amount of memory usage used to hold the dataframe in a readable human syntax.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#info-columns-3">info_columns/3</a></td>
    <td>gets the information of each column in a dataframe in a readable human syntax.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#selection-4">selection/4</a></td>
    <td>selects only certain columns from the dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#plot-4">plot/4</a></td>
    <td>generates a plot from dataframe using keywords to spec args for graph.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#groupby-4">groupby/4</a></td>
    <td>groups a dataframe based on passed columns.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#apply-4">apply/4</a></td>
    <td>applies a function in a dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#sort-values-4">sort_values/4</a></td>
    <td>sorts a dataframe by specific a column.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#sort-index-4">sort_index/4</a></td>
    <td>sorts a dataframe or groupby using index.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#legacy-query-4">legacy_query/4</a></td>
    <td>applies a query as `query` but it can be used for more complex queries such as string comparisons.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#legacy-assignment-4">legacy_assignment/4</a></td>
    <td>assign a series to a new column in a dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#drop-4">drop/4</a></td>
    <td>drops a column in a dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#rename-4">rename/4</a></td>
    <td>rename a column in a dataframe.</td>    
  </tr>      
  <tr>
    <td valign="top"><a href="#read-string-3">read_string/3</a></td>
    <td>reads a string as a csv into a valid dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#append-4">append/4</a></td>
    <td>appends a dataframe in another dataframe (mix).</td>
  </tr>
  <tr>
    <td valign="top"><a href="#update-4">update/4</a></td>
    <td>updates a dataframe with another dataframe by align indexes.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#set-index-4">set_index/4</a></td>
    <td>sets index in a dataframe.</td>
  </tr>
  <tr>
    <td valign="top"><a href="#reset-index-4">reset_index/4</a></td>
    <td>resets index in a dataframe</td>
  </tr>
  <tr>
    <td valign="top"><a href="#drop-duplicates-4">drop_duplicates/4</a></td>
    <td>drops rows duplicated in a dataframe following the criteria passed over keywords</td>
  </tr>
  <tr>
    <td valign="top"><a href="#fillna-4">fillna/4</a></td>
    <td>fills na values</td>
  </tr>
  <tr>
    <td valign="top"><a href="#dropna-4">dropna/4</a></td>
    <td>drops na values</td>
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
max(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the max value of a column or in an entire grouped dataframe.

<a name="min-4"></a>

### min/4 ###

<pre><code>
min(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the min value of a column or in an entire grouped dataframe.

<a name="count-4"></a>

### count/4 ###

<pre><code>
count(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the count of a column or in an entire grouped dataframe.

<a name="median-4"></a>

### median/4 ###

<pre><code>
median(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the median value of a column or in an entire grouped dataframe.

<a name="sum-4"></a>

### sum/4 ###

<pre><code>
sum(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, number()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

calculates the sum value of a column or in an entire grouped dataframe.

<a name="unique-4"></a>

### unique/4 ###

<pre><code>
unique(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, binary()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

return the uniques data of a column in a dataframe.

<a name="read-csv-2"></a>

### read_csv/2 ###

<pre><code>
read_csv(<a href="#type-jun-worker">jun_worker()</a>, path :: atom()) -> {ok, <a href="#type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

read a csv and transforms into a valid dataframe.

<a name="to-csv-3"></a>

### to_csv/3 ###

<pre><code>
to_csv(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, csv :: string()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

transforms a dataframe into a csv format.

<a name="to-html-3"></a>

### to_html/3 ###

<pre><code>
to_html(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, html :: string()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

transforms a dataframe into an html format.

<a name="to-json-3"></a>

### to_json/3 ###

<pre><code>
to_json(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, json :: string()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

transforms a dataframe into a json format.

<a name="to-datetime-3"></a>

### to_datetime/3 ###

<pre><code>
to_datetime(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-series">series()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, series :: jun_series()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

transforms a column of dataframe into a valid datetime dtype.

<a name="read-sql-2"></a>

### read_sql/2 ###

<pre><code>
read_sql(<a href="#type-jun-worker">jun_worker()</a>, sql_query :: atom()) -> {ok, <a href="#type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

read a sql query and transforms into a valid dataframe.

<a name="query-4"></a>

### query/4 ###

<pre><code>
query(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, query :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="#type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

execute a valid query into dataframe.

<a name="head-4"></a>

### head/4 ###

<pre><code>
head(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, n :: non_neg_integer(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="#type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

gets the N first elements.


<a name="tail-4"></a>

### tail/4 ###

<pre><code>
tail(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, n :: non_neg_integer(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="#type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

gets the N last elements.

<a name="columns-3"></a>

### columns/3 ###

<pre><code>
columns(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, columns :: binary()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

gets the column names as binary separated by comma in dataframe.

<a name="len-columns-3"></a>

### len_columns/3 ###

<pre><code>
len_columns(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, length :: non_neg_integer()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

gets the total number of columns in dataframe.

<a name="len-index-3"></a>

### len_index/3 ###

<pre><code>
len_index(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, length :: non_neg_integer()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

gets the length of index in a dataframe.

<a name="memory-usage-3"></a>

### memory_usage/3 ###

<pre><code>
memory_usage(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, binary()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

gets the amount of memory usage used to hold the dataframe in a readable human syntax.

<a name="info-columns-3"></a>

### info_columns/3 ###

<pre><code>
info_columns(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, binary()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

gets the information of each column in a dataframe in a readable human syntax. 

<a name="selection-4"></a>

### selection/4 ###

<pre><code>
selection(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, columns :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, binary()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

selects only certain columns from the dataframe, you can pass the third argument as an atom separated by comma.

<a name="plot-4"></a>

### plot/4 ###

<pre><code>
plot(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, path :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, jun_subplot()} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

generates a plot from dataframe using keywords to spec args for graph, it will be saved at `path` if passed otherwise just in memory.

<a name="groupby-4"></a>

### groupby/4 ###

<pre><code>
groupby(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, columns :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-groupby">jun_groupby()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

groups a dataframe based on passed columns.

<a name="apply-4"></a>

### apply/4 ###

<pre><code>
apply(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-series">jun_series()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

applies a function in a dataframe and return a series. see <a href="type-lambda">lambda</a> to check how to express a function in erlang.

<a name="sort-values-4"></a>

### sort_values/4 ###

<pre><code>
sort_values(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, 'None', <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

sorts a dataframe by specific a column.

<a name="sort-index-4"></a>

### sort_index/4 ###

<pre><code>
sort_index(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, 'None', <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

sorts a dataframe or groupby using index.

<a name="legacy-query-4"></a>

### legacy_query/4 ###

<pre><code>
legacy_query(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, query :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

applies a query as `query` but it can be used for more complex queries such as string comparisons.

<a name="legacy-assignment-4"></a>

### legacy_assignment/4 ###

<pre><code>
legacy_assignment(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-series">series()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

assign a series to a new column in a dataframe.

<a name="drop-4"></a>

### drop/4 ###

<pre><code>
drop(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

drops a column in a dataframe.

<a name="rename-4"></a>

### rename/4 ###

<pre><code>
rename(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: 'None', <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

renames a column in a dataframe.

<a name="read-string-3"></a>

### read_string/3 ###

<pre><code>
read_string(<a href="#type-jun-worker">jun_worker()</a>, string :: string(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

reads a string as a csv into a valid dataframe.

<a name="append-4"></a>

### append/4 ###

<pre><code>
append(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

appends a dataframe in another dataframe (mix).

### update/4 ###

<pre><code>
update(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-dataframe">dataframe()</a>, <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

updates a dataframe with another dataframe by align indexes.

### set_index/4 ###

<pre><code>
set_index(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: atom(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

sets index in a dataframe.

### reset_index/4 ###

<pre><code>
reset_index(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, 'None', <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

resets index in a dataframe.

### drop_duplicates/4 ###

<pre><code>
drop_duplicates(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, column :: string(), <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

drops rows duplicated in a dataframe following the criteria passed over keywords.

### fillna/4 ###

<pre><code>
fillna(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, 'None', <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

fills na values.

### dropna/4 ###

<pre><code>
dropna(<a href="#type-jun-worker">jun_worker()</a>, <a href="#type-dataframe">dataframe()</a>, 'None', <a href="#type-keywords">keywords()</a>) -> {ok, <a href="type-jun-dataframe">jun_dataframe()</a>} | <a href="#type-jun-error">jun_error()</a>
</code></pre>

drops na values.
