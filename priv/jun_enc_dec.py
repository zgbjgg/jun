#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
from erlport.erlterms import Atom, List
from erlport.erlang import set_encoder, set_decoder

def setup_dtype():
    set_encoder(dtype_encoder)
    set_decoder(dtype_decoder)
    return Atom(b'ok')

def dtype_encoder(value):
    if isinstance(value, np.int64):
        return np.asscalar(value)
    elif isinstance(value, np.float64):
        return np.asscalar(value)
    elif isinstance(value, str):
        try:
            return value.encode('utf-8') # to express as binary() instead of string() on erlang side
        except:
            return value
    elif isinstance(value, list):
        return [dtype_encoder(v) for v in value]
    elif isinstance(value, tuple):
        nvalue = ()
        for v in value:
            nvalue = nvalue + (dtype_encoder(v),)
        return nvalue
    else:
        try:
            return value.encode('utf-8')
        except:
            return value

def dtype_decoder(value):
    try:
        if isinstance(value, List):
            return [dtype_decoder(v) for v in value]
        elif isinstance(value, tuple):
            nvalue = ()
            for v in value:
                nvalue = nvalue + (dtype_decoder(v),)
            return nvalue
        elif isinstance(value, str):
            return value
        else:
            return value.decode("utf-8")
    except:
        return value
