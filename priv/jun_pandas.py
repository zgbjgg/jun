import sys
import scipy as sp
import numpy as np
import matplotlib as mpl
import pandas as pd
import sklearn as skl
from erlport.erlterms import Atom

# simple return sys version
def version():
    return sys.version

# descriptive stats over a dataframe
# this is used with a dynamical assignment since
# data frame will hold by erlang process and the
# syntax to apply functions over data will be complex
def jun_dataframe(df, fn, args, axis='None', keywords=[]):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        if axis != 'None':
            fun = getattr(df[axis], fn)
        else:
            fun = getattr(df, fn)
        # make dict from keywords even if empty!
        kwargs = dict(keywords)
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
def columns(df):
    if isinstance(df, pd.core.frame.DataFrame):
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
