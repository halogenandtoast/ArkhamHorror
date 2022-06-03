module Arkham.Classes.HasTokenValue
  ( module Arkham.Classes.HasTokenValue
  ) where

import Arkham.Prelude

import Arkham.InvestigatorId
import Arkham.Projection
import Arkham.Location.Attrs
import Arkham.Token

class HasTokenValue m a where
  getTokenValue :: Projection m LocationAttrs => InvestigatorId -> TokenFace -> a -> m TokenValue
