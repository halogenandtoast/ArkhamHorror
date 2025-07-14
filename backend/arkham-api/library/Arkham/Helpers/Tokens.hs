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
     , QueryElement matcher ~ el
     , HasTokens el
     , Projection (HasTokensRoot el)
     , EntityId (HasTokensRoot el) ~ el
     )
  => Token -> matcher -> m Int
selectCountTokens tkn matcher = selectSumWith (countTokens tkn) (tokenField @el) matcher
