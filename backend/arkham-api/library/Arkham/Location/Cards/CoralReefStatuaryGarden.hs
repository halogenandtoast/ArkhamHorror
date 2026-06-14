module Arkham.Location.Cards.CoralReefStatuaryGarden (coralReefStatuaryGarden) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CoralReefStatuaryGarden = CoralReefStatuaryGarden LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coralReefStatuaryGarden :: LocationCard CoralReefStatuaryGarden
coralReefStatuaryGarden = location CoralReefStatuaryGarden Cards.coralReefStatuaryGarden 4 (Static 3)

instance HasAbilities CoralReefStatuaryGarden where
  getAbilities (CoralReefStatuaryGarden a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here doubleActionAbility

instance RunMessage CoralReefStatuaryGarden where
  runMessage msg l@(CoralReefStatuaryGarden attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 2
      gainResources iid (attrs.ability 1) 2
      createEnemyAt_ Enemies.underseaParasite attrs
      pure l
    -- TODO: back side "Forced - When you enter Sea Floor: Increase the flood
    -- level of this location and an adjacent eligible location." (shared
    -- unrevealed Seafloor mechanic, not yet implemented for any Seafloor location)
    _ -> CoralReefStatuaryGarden <$> liftRunMessage msg attrs
