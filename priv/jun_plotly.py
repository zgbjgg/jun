#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import scipy as sp
import numpy as np
import pandas as pd
import plotly.plotly as py
import cufflinks as cf
from erlport.erlterms import Atom

# define a global dict to store items that not working pickled
pickling = {}

# simple return sys version
def version():
    return sys.version

# common helper for plotting functions in plotly
# using cufflinks, wrapped over erlang,
# return the url holding the plot to use in the client 
def jun_iplot_dataframe(df, key, keywords=[]):
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
        pickling[key] = iplot
        # use get data since this should be achive as figure
        return (Atom("plotly.iplot"), key)
    else:
        return 'error_format_data_frame_invalid'

def jun_iplot_plot(iplot, filename, keywords=[]):
    iplot = pickling[iplot]
    url = py.plot(iplot, filename=filename, auto_open=False)
    return url

def jun_iplot_extend(iplot_x, iplot_y, keywords=[]):
    iplot_yy = pickling[iplot_y]
    iplot_xx = pickling[iplot_x]
    iplot_yy['data'].extend(iplot_xx['data'])
    pickling[iplot_y] = iplot_yy
    return (Atom("plotly.iplot"), iplot_y)

def jun_iplot_get_figure(url, key, keywords=[]):
    iplot = py.get_figure(url)
    pickling[key] = iplot
    return (Atom("plotly.iplot"), key)
