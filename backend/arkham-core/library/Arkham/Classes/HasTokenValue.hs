module Arkham.Classes.HasTokenValue
  ( module Arkham.Classes.HasTokenValue
  ) where

import Arkham.Prelude

import Arkham.InvestigatorId
import Arkham.Projection
import Arkham.Location.Attrs
import Arkham.Token

type TokenValueM env m = (MonadReader env m, Projection env LocationAttrs)

class HasTokenValue env a where
  getTokenValue :: TokenValueM env m => InvestigatorId -> TokenFace -> a -> m TokenValue
