module Errors where

import Control.Monad.Except
import qualified Data.Text as T

-- TODO: refactor to two classes of errors:
--   * runtime errors (exceptions)
--   * interpreter (internal) errors
data Error
  = NonBoolInIf
  | NonBoolInWhile
  | SettingUndefinedVar T.Text
  | ReferencingUndefinedVar T.Text
  | AppliedNonFunc
  | ProblemInPrimitive
  | ParamsNotUnique T.Text
  | ReferencedUnspecified
    -- ^ probably in a letrec

  | InternalInconsistency T.Text
  deriving(Show)
