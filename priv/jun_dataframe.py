import pandas
import numpy
from erlport.erlterms import Atom
from erlport.erlang import set_encoder, set_decoder

def setup_dict_type():
    set_decoder(dict_decoder)
    set_encoder(dict_encoder)
    return Atom("ok")

def dict_encoder(value):
    if isinstance(value, pandas.core.frame.DataFrame):
        dt = value.to_dict()        
        value = ()
        for key in dt:
            tkey = Atom(key)
            tvalue = ()
            for subkey in dt[key]:
                tvalue = tvalue + (dtype(dt[key][subkey]),)
            value = value + ((tkey, tvalue),)                 
    return value 

def dict_decoder(value):
    dictionary = {}
    if isinstance(value, tuple):
        for item in value:
            dictionary_value = {}
            if isinstance(item[1], tuple):
                for index, valuable in enumerate(item[1]):
                    dictionary_value[index] = valuable
            dictionary[item[0]] = dictionary_value
        return dictionary
    else:
        return value

def dtype(value):
    if isinstance(value, numpy.int64):
        value = numpy.asscalar(value) 
    return value
