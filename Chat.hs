module Chat where

import           Data.Time.LocalTime (ZonedTime)

type Msg = (Int,ZonedTime,String,String)
