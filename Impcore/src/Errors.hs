module Errors where

import Control.Monad.Except

data Error
  = ErrorString String
  | ErrorUnspecified
  deriving(Show)
