module Arkham.Location.Cards.VelmasDiner (velmasDiner, VelmasDiner (..)) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype VelmasDiner = VelmasDiner LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

velmasDiner :: LocationCard VelmasDiner
velmasDiner = location VelmasDiner Cards.velmasDiner 2 (Static 0)

instance HasModifiersFor VelmasDiner where
  getModifiersFor (VelmasDiner a) = do
    modifySelectMap
      a
      (locationIs Cards.easttown)
      \lid -> [ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)]

instance HasAbilities VelmasDiner where
  getAbilities (VelmasDiner attrs) =
    extendRevealed1 attrs
      $ playerLimit PerGame
      $ restrictedAbility attrs 1 Here
      $ ActionAbility []
      $ ActionCost 3

instance RunMessage VelmasDiner where
  runMessage msg l@(VelmasDiner attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ GainClues iid (toAbilitySource attrs 1) 2
      pure l
    _ -> VelmasDiner <$> runMessage msg attrs
