module Struct.Interface where

import Data.Map as Map

import Struct.Manager as Manager
import Struct.Program as Program
import Struct.System  as System
import Struct.DBMS    as DBMS

data Interface = Interface
    { manager  :: Manager Program 
    , database :: Map Program (DBMS String) }

select :: Interface -> Maybe (DBMS String)
select i = manager i >>= flip (Map.lookup . vert) (database i)

