{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Attic where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype Attic = Attic Attrs
  deriving newtype (Show, ToJSON, FromJSON)

attic :: Attic
attic = Attic $ (baseAttrs "01113" "Attic" 1 (PerPlayer 2) Triangle [Square] []
                )
  { locationVictory = Just 1
  }

instance HasModifiersFor env Attic where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Attic where
  getActions i window (Attic attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Attic where
  runMessage msg a@(Attic attrs@Attrs { locationId }) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> a <$ unshiftMessage
      (InvestigatorAssignDamage iid (LocationSource locationId) 0 1)
    _ -> Attic <$> runMessage msg attrs
