#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import scipy as sp
import numpy as np
import matplotlib as mpl
import pandas as pd
import sklearn as skl
import operator as opt
import pyodbc as pyodbc
from pandas.compat import StringIO
from erlport.erlterms import Atom
from dateutil.parser import parse
mpl.use('Agg')
opers = {'<': opt.lt,
         '>': opt.gt,
         '<=': opt.le,
         '>=': opt.ge,
         '==': opt.eq,
         '!=': opt.ne}
sql = [ 'dsn', 'username', 'password', 'database']

# simple return sys version
def version():
    return sys.version

# descriptive stats over a dataframe
# this is used with a dynamical assignment since
# data frame will hold by erlang process and the
# syntax to apply functions over data will be complex
def jun_dataframe(df, fn, args, axis='None', keywords=[]):
    if ( isinstance(df, pd.core.frame.DataFrame) | isinstance(df, pd.core.groupby.DataFrameGroupBy) ):
        args = [ islambda_from_erl(arg) for arg in args ]
        if axis != 'None':
            fun = getattr(df[axis], fn)
        else:
            fun = getattr(df, fn)
        # make dict from keywords even if empty!
        kwargs = dict([ (k, isexpression_from_erl(v)) for (k, v) in keywords ])
        # explicity execute the fun
        if len(args) == 0:
            value = fun(**kwargs)
        else:
            value = fun(*args, **kwargs)
        # check for instance of int64 and return as scalar
        if isinstance(value, np.int64): 
            return np.asscalar(value)
        elif isinstance(value, np.float64):
            return np.asscalar(value)
        elif isinstance(value, pd.core.frame.DataFrame):
            return (Atom("pandas.core.frame.DataFrame"), value)
        elif isinstance(value, pd.core.groupby.DataFrameGroupBy):
            return (Atom("pandas.core.groupby.DataFrameGroupBy"), value)
        elif isinstance(value, pd.core.frame.Series):
            return (Atom("pandas.core.frame.Series"), value)
        elif isinstance(value, np.ndarray):
            return ','.join(str(v) for v in value)
        elif value is None: # commonly when fun applies over callable object
            return (Atom("pandas.core.frame.DataFrame"), df)
        else:
            return value
    else:
        return 'error_format_data_frame_invalid'

# a common function to decode the pandas dataframe into
# a readable erlang term
def to_erl(value):
    if isinstance(value, pd.core.frame.DataFrame):
        # fill NaN as default since term cannot back to py
        jundataframe = value.fillna('NaN').values.tolist()
        columns = list(value)
        return (Atom("pandas.core.frame.DataFrame"), columns, jundataframe)
    else:
        return 'error_formar_data_frame_invalid'

# working with columns are a important feature, but the main
# function cannot deal with that, so just add a specific fn to that
# When using multiIndex ensure index names!
def columns(df):
    if isinstance(df, pd.core.frame.DataFrame):
        if isinstance(df.index, pd.core.index.MultiIndex):
            columns = list(df) + list(df.index.names)
        else:
            columns = list(df)
        columns_as_str = ','.join(columns)
        return columns_as_str
    else:
        return 'error_format_data_frame_invalid'

# len columns helper
def len_columns(df):
    if isinstance(df, pd.core.frame.DataFrame):
        return len(df.columns)                    
    else:
        return 'error_format_data_frame_invalid'

# len index helper
def len_index(df):
    if isinstance(df, pd.core.frame.DataFrame):
        return len(df.index)
    else:
        return 'error_format_data_frame_invalid'

# memory usage helper
def memory_usage(df):
    if isinstance(df, pd.core.frame.DataFrame):
        num = df.memory_usage(index=True, deep=True).sum()
        return _sizeof_fmt(num)
    else:
        return 'error_format_data_frame_invalid'

# columns description (in a csv format) helper
def info_columns(df):
    if isinstance(df, pd.core.frame.DataFrame):
        lines = ""
        counts = df.count() # for non-null values
        for i, column in enumerate(df.columns):
            dtype = df.dtypes.iloc[i]
            nonnull = counts.iloc[i]
            lines = lines + "%s,%s,%s\n" % (column, dtype, nonnull)
        return lines
    else:
        return 'error_format_data_frame_invalid'

# size into human readable, taken from:
# https://github.com/pandas-dev/pandas/blob/master/pandas/core/frame.py
def _sizeof_fmt(num):
    # returns size in human readable format
    for x in ['bytes', 'KB', 'MB', 'GB', 'TB']:
        if num < 1024.0:
            return "%3.1f%s %s" % (num, '+', x)
        num /= 1024.0
    return "%3.1f%s %s" % (num, '+', 'PB')

# common helper for plotting functions, wrapped over
# erlang, declare if outputs goes to a path (image) or
# only holds into memory as a single py dtype
def jun_dataframe_plot(df, save='None', keywords=[]):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        # make dict from keywords even if empty!
        kwargs = dict([ (k, isexpression_from_erl(v)) for (k, v) in keywords ])
        # IMPORTANT: check if columns has the x and y, otherwise remove to plot
        x = kwargs.get('x', 'None')
        y = kwargs.get('y', 'None')
        columns = list(df)
        if x not in columns and x != 'None':
          del kwargs['x']
        if y not in columns and y != 'None':
          del kwargs['y']
        # explicity execute the fun
        plot = df.plot(**kwargs)
        if save != 'None':
            fig = plot.get_figure()
            fig.savefig(save, bbox_inches='tight') # save contains path
            return 'matplotlib.AxesSubplot'
        else:
            return (Atom("matplotlib.AxesSubplot"), plot) # this is correct? because can be confusing with opaque df
    else:
        return 'error_format_data_frame_invalid'

# common selection of columns (slicing)
# this can be acomplished using loc but it's better
# using a single syntax such as accesing data from dataframe
def selection(df, columns):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        return (Atom("pandas.core.frame.DataFrame"), df[list(columns)])
    else:
        return 'error_format_data_frame_invalid'

# since query function cannot evaluate columns
# with spaces in it (or even values) just use the legacy query
# as a single filter, check for strings comparison as contains
def legacy_query(df, column, operand, value):
    if ( isinstance(df, pd.core.frame.DataFrame) | isinstance(df, pd.core.groupby.DataFrameGroupBy) ):
        operation = opers[operand]
        if isinstance(value, str):
            try:
                parse(value, False)
                newdf = df[operation(df[column], value)]
            except ValueError:
                newdf = df[df[column].str.contains(value)] # since str cannot be evaluated with '=='
        else:
            newdf = df[operation(df[column], value)]

        if isinstance(newdf, pd.core.frame.DataFrame):
            return (Atom("pandas.core.frame.DataFrame"), newdf)
        elif isinstance(newdf, pd.core.groupby.DataFrameGroupBy):
            return (Atom("pandas.core.groupby.DataFrameGroupBy"), newdf)
        else:
            return newdf
    else:
        return 'error_format_data_frame_invalid'

# simple receiver for a lambda in string mode and pass
# back to opaque term, it means a valid evaluated lambda
# into py environment
def islambda_from_erl(fn):
    try:
        fn0 = eval(fn)
        if ( callable(fn0) and fn0.__name__ == '<lambda>' ):
            return fn0
        else:
            raise Exception('err.invalid.jun.Lambda', 'not a lambda valid function from erl instance')
    except:
        return fn # return fn safetly, same as passed

# simple assignment for a serie to a dataframe as column or
# even other assignments, but do it from here since cannot be evaluated
# outside due to py syntax
def legacy_assignment(df, column, value):
    if ( isinstance(df, pd.core.frame.DataFrame) | isinstance(df, pd.core.groupby.DataFrameGroupBy) ):
        df[column] = value
        if isinstance(df, pd.core.frame.DataFrame):
            return (Atom("pandas.core.frame.DataFrame"), df)
        elif isinstance(df, pd.core.groupby.DataFrameGroupBy):
            return (Atom("pandas.core.groupby.DataFrameGroupBy"), df)
        else:
            return df
    else:
        return 'error_format_data_frame_invalid'

# descriptive stats over a series
# this is used with a dynamical assignment since
# series will hold by erlang process and the
# syntax to apply functions over data will be complex
def jun_series(series, fn, args, axis='None', keywords=[]):
    if ( isinstance(series, pd.core.frame.Series) ):
        args = [ islambda_from_erl(arg) for arg in args ]
        keywords = [ (k, islambda_from_erl(v)) for (k, v) in keywords ]
        # for complete integration also check for keywords with lambdas
        # keywords = [ islambda_from_erl(keyword) for keyword in keywords ]
        if axis != 'None':
            fun = getattr(series[axis], fn)
        else:
            fun = getattr(series, fn)
        # make dict from keywords even if empty!
        kwargs = dict([ (k, isexpression_from_erl(v)) for (k, v) in keywords ])
        # explicity execute the fun
        if len(args) == 0:
            value = fun(**kwargs)
        else:
            value = fun(*args, **kwargs)
        # check for instance of int64 and return as scalar
        if isinstance(value, np.int64):
            return np.asscalar(value)
        elif isinstance(value, np.float64):
            return np.asscalar(value)
        elif isinstance(value, pd.core.frame.Series):
            return (Atom("pandas.core.frame.Series"), value)
        elif isinstance(value, np.ndarray):
            return ','.join(str(v) for v in value)
        else:
            return value
    else:
        return 'error_format_series_invalid'

# in keywords we cannot assing so easily the data comming from
# erlang, since protocol buffers keep keywords as single strings
# in key and value, eval each value so we can check if some of them
# must be evaluated as an expression
def isexpression_from_erl(expression):
    try:
        return eval(expression)
    except:
        return expression # return fn safetly, same as passed

# this is used to apply functions to a
# single dataframe but instead of getting functions to apply
# based on the DataFrame class, use directly pandas implementation
def jun_pandas(fn, args, keywords=[]):
    args = [ islambda_from_erl(arg) for arg in args ]
    fun = getattr(pd, fn)
    # we need to check the conn if using `read_sql` since conn
    # cannot be pickled :-(
    if fn == 'read_sql':
        keywords.extend([('con', conn(keywords, sql))])
    # make dict from keywords even if empty!
    kwargs = dict([ (k, isexpression_from_erl(v)) for (k, v) in keywords if not k in sql])
    # explicity execute the fun
    if len(args) == 0:
        value = fun(**kwargs)
    else:
        value = fun(*args, **kwargs)
    # check for instance of int64 and return as scalar
    if isinstance(value, np.int64):
        return np.asscalar(value)
    elif isinstance(value, np.float64):
        return np.asscalar(value)
    elif isinstance(value, pd.core.frame.DataFrame):
       return (Atom("pandas.core.frame.DataFrame"), value)
    elif isinstance(value, pd.core.groupby.DataFrameGroupBy):
        return (Atom("pandas.core.groupby.DataFrameGroupBy"), value)
    elif isinstance(value, pd.core.frame.Series):
        return (Atom("pandas.core.frame.Series"), value)
    elif isinstance(value, np.ndarray):
        return ','.join(str(v) for v in value)
    else:
        return value

# single selection of a column, return a series data
# since this are now supported by jun core
def single_selection(df, column):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        return (Atom("pandas.core.frame.Series"), df[column])
    else:
        return 'error_format_data_frame_invalid'

# timedelta operations
def jun_timedelta(series, fn, axis='None', keywords=[]):
    if isinstance(series, pd.core.frame.Series):
        fun = getattr(series, 'dt')
        value = getattr(fun, fn)
        if isinstance(value, pd.core.frame.Series):
            return (Atom("pandas.core.frame.Series"), value)
        else:
            return value
    else:
        return 'error_format_data_frame_or_serie_invalid'

# make a valid connection for sql using
# pyodbc, this will return an opaque connection
# to erlang, but the jun_pandas module will use that
# to use related functions using such connection.
def conn(keywords, sql):
    # ensure that ini file exists to generate the connection
    for (k, v) in keywords:
        if k in sql:
            if k == 'dsn':
                dsn = 'DSN=' + v
            elif k == 'username':
                username = 'UID=' + v
            elif k == 'password':
                password = 'PWD=' + v
            elif k == 'database':
                database = 'DATABASE=' + v
    setup_conn = ( dsn, username, password, database )
    conn = pyodbc.connect(';'.join(str(arg) for arg in setup_conn))
    return conn

# custom fun to treat string as a new dataframe
# this from python is a custom code so JUN implements directly from
# its API
def read_string(string, keywords):
    args = [StringIO(string.to_string())]
    fun = getattr(pd, 'read_csv')
    kwargs = dict(keywords)
    # explicity execute the fun
    value = fun(*args, **kwargs)
    return (Atom("pandas.core.frame.DataFrame"), value)
