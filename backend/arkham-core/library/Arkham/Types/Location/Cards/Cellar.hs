{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Cellar where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import ClassyPrelude

newtype Cellar = Cellar Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cellar :: Cellar
cellar = Cellar $ (baseAttrs "01114" "Cellar" 4 (PerPlayer 2) Plus [Square] [])
  { locationVictory = Just 1
  }

instance (IsInvestigator investigator) => HasActions env investigator Cellar where
  getActions i window (Cellar attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Cellar where
  runMessage msg a@(Cellar attrs@Attrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      unshiftMessage
        $ InvestigatorAssignDamage iid (LocationSource locationId) 1 0
      pure a
    _ -> Cellar <$> runMessage msg attrs
