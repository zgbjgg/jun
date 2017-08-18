import pandas as pd
import numpy as np
from erlport.erlterms import Atom
from erlport.erlang import set_encoder

def setup_dtype():
    set_encoder(dtype_encoder)
    return Atom("ok")

def dtype_encoder(value):
    if isinstance(value, np.int64):
        return np.asscalar(value)
    elif isinstance(value, np.float64):
        return np.asscalar(value)
    elif isinstance(value, pd.core.frame.DataFrame):
        return (Atom("pandas.core.frame.DataFrame"), value)
    else:
        return value
