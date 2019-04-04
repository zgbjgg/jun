#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import scipy as sp
import numpy as np
import matplotlib as mpl
import pandas as pd
import sklearn as skl
from erlport.erlterms import Atom
mpl.use('Agg')
import seaborn as sns

# common helper for dataframe plot using seaborn,
# trying to return a file instead a raw opaque item
def jun_dataframe_plot(df, fn, save='None', keywords=[]):
    if ( isinstance(df, pd.core.frame.DataFrame) ):
        # clean before any plot
        mpl.pyplot.figure()
        # make dict from keywords even if empty!
        kwargs = dict(keywords)
        # get the fun from seaborn directly since we want a dynamic call
        fun = getattr(sns, fn)
        plot = fun(**kwargs)
        if ( plot.__class__.__name__ == 'AxesSubplot' ):
            plot_class = 'matplotlib.AxesSubplot'
        else:
            plot_class = 'seaborn.axisgrid.*'
        if save != 'None':
            # if figure comes from seaborn use fig, otherwise get_figure
            if ( plot_class == 'matplotlib.AxesSubplot' ):
                fig = plot.get_figure()
            else:             
                fig = plot.fig
            fig.savefig(save, bbox_inches='tight') # save contains path
            return plot_class
        else:
            return (Atom(plot_class), plot) # this is correct? because can be confusing with opaque df
    else:
        return 'error_format_data_frame_invalid'
