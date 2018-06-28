import pyodbc as pyodbc
from erlport.erlterms import Atom

# make a valid connection for sql using
# pyodbc, this will return an opaque connection
# to erlang, but the jun_pandas module will use that
# to use related functions using such connection.
def conn(dsn, username, password, database):
    # ensure that ini file exists to generate the connection
    dsn = 'DSN=' + dsn
    username = 'UID=' + username
    password = 'PWD=' + password
    database = 'DATABASE=' + database
    setup_conn = ( dsn, username, password, database )
    conn = pyodbc.connect(';'.join(str(arg) for arg in setup_conn))
    return (Atom("pyodbc.conn"), conn)
