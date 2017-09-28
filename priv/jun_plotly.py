import sys
import scipy as sp
import numpy as np
import pandas as pd
import plotly.plotly as py
import cufflinks as cf

# simple return sys version
def version():
    return sys.version

# common helper for plotting functions in plotly
# using cufflinks, wrapped over erlang,
# return the url holding the plot to use in the client 
def jun_dataframe_iplot(df, _filename, keywords=[]):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        # make dict from keywords even if empty!
        kwargs = dict(keywords)
        # IMPORTANT: check if columns has the x and y, otherwise remove to plot
        x = kwargs.get('x')
        y = kwargs.get('y')
        columns = list(df)
        if x not in columns:
          del kwargs['x']
        if y not in columns:
          del kwargs['y']
        # explicity execute the fun
        iplot = df.iplot(**kwargs)
        # since iplot is an object of plotly.tools.PlotlyDisplay class, then make as plot
        url = py.plot(iplot, auto_open=False) 
        return url
    else:
        return 'error_format_data_frame_invalid'
