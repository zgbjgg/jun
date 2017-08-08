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
