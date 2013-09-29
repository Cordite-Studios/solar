{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solar.Data.Cereal where


import Data.Serialize as S
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad
import Data.Text


instance Serialize Day where
    get = liftM3 fromGregorian get get get
    put d = put year >> put month >> put day
        where
            (year, month, day) = toGregorian d
instance Serialize DiffTime where
    get = liftM fromRational get
    put d = put (toRational d)

instance Serialize UTCTime where
    get = liftM2 UTCTime get get
    put (UTCTime day time) = put day >> put time
instance Serialize Text where
    get = liftM pack get
    put t = put (unpack t)