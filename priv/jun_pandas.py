import sys
import scipy as sp
import numpy as np
import matplotlib as mpl
import pandas as pd
import sklearn as skl

# simple return sys version
def version():
    return sys.version

# descriptive stats over a dataframe
# this is used with a dynamical assignment since
# data frame will hold by erlang process and the
# syntax to apply functions over data will be complex
def df_stats(df, axis, fn):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        fun = getattr(df[axis], fn)
        # explicity execute the fun
        value = fun()
        # check for instance of int64 and return as scalar
        if isinstance(value, np.int64): 
            return np.asscalar(fun())
        else:
            return value
    else:
        return 'error_format_data_frame_invalid'

# a single wrapper to render dataframe in a legible
# format such a csv
def df_to_csv(df):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        csv = df.to_csv()
        return csv
    else:
        return 'error_format_data_frame_invalid'
