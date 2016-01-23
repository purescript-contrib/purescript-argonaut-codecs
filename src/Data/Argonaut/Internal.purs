module Data.Argonaut.Internal where

import Prelude
import Data.Foldable (foldr)
import Data.String (lastIndexOf, drop)
import Data.Generic (DataConstructor())
import Data.Array (null)
import Data.Maybe (Maybe(..))

allConstrNullary :: Array DataConstructor -> Boolean
allConstrNullary constrSigns = foldr (&&) true <<< map (\c -> null c.sigValues) $ constrSigns

fixConstr :: String -> String
fixConstr constr = case lastIndexOf "." constr of
                    Nothing -> constr
                    Just i -> drop (i+1) constr
