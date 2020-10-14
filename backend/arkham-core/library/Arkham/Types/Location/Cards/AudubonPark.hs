{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.AudubonPark where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype AudubonPark = AudubonPark Attrs
  deriving newtype (Show, ToJSON, FromJSON)

audubonPark :: AudubonPark
audubonPark = AudubonPark $ (baseAttrs
                              "81011"
                              "Audubon Park"
                              3
                              (PerPlayer 1)
                              Squiggle
                              [Triangle, Squiggle]
                              [Riverside]
                            )
  { locationVictory = Just 1
  }

instance HasModifiersFor env investigator AudubonPark where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator AudubonPark where
  getActions i window (AudubonPark attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env AudubonPark where
  runMessage msg l@(AudubonPark attrs@Attrs {..}) = case msg of
    EnemyEvaded iid eid | eid `member` locationEnemies ->
      l <$ unshiftMessage (RandomDiscard iid)
    _ -> AudubonPark <$> runMessage msg attrs
