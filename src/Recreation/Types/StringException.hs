module Recreation.Types.StringException where

import Control.Exception (Exception)

newtype StringException = StringException String

instance Show StringException where
  show (StringException msg) = msg

instance Exception StringException

make :: String -> StringException
make = StringException
