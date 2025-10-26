module Arkham.Helpers.Tokens where

import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Field
import Arkham.Id
import Arkham.Location.Types
import Arkham.Prelude
import Arkham.Projection
import Arkham.Query
import Arkham.Token
import Arkham.Tracing

class HasTokens a where
  type HasTokensRoot a :: Type
  tokenField :: Field (HasTokensRoot a) Tokens

instance HasTokens LocationId where
  type HasTokensRoot LocationId = Location
  tokenField = LocationTokens

selectCountTokens
  :: forall m matcher el
   . ( Query matcher
     , HasGame m
     , Tracing m
     , QueryElement matcher ~ el
     , HasTokens el
     , Projection (HasTokensRoot el)
     , EntityId (HasTokensRoot el) ~ el
     )
  => Token -> matcher -> m Int
selectCountTokens tkn matcher = selectSumWith (countTokens tkn) (tokenField @el) matcher

countTokensOf
  :: forall a m
   . ( HasGame m
     , Tracing m
     , EntityId (HasTokensRoot a) ~ IdOf a
     , AsId a
     , HasTokens a
     , Projection (HasTokensRoot a)
     )
  => Token -> a -> m Int
countTokensOf tkn a = fieldMap (tokenField @a) (countTokens tkn) (asId a)
