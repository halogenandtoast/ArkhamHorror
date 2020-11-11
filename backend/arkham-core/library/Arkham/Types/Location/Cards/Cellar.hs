{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Cellar where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Cellar = Cellar Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cellar :: Cellar
cellar = Cellar $ (baseAttrs "01114" "Cellar" 4 (PerPlayer 2) Plus [Square] [])
  { locationVictory = Just 1
  }

instance HasModifiersFor env Cellar where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Cellar where
  getActions i window (Cellar attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Cellar where
  runMessage msg a@(Cellar attrs@Attrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId ->
      a <$ unshiftMessage (InvestigatorAssignDamage iid (toSource attrs) 1 0)
    _ -> Cellar <$> runMessage msg attrs
