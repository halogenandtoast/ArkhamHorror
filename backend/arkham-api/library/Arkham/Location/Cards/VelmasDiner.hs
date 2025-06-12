module Arkham.Location.Cards.VelmasDiner (velmasDiner) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype VelmasDiner = VelmasDiner LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

velmasDiner :: LocationCard VelmasDiner
velmasDiner = location VelmasDiner Cards.velmasDiner 2 (Static 0)

instance HasModifiersFor VelmasDiner where
  getModifiersFor (VelmasDiner a) = do
    modifySelectMap a (locationIs Cards.easttown) \lid -> [ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)]

instance HasAbilities VelmasDiner where
  getAbilities (VelmasDiner attrs) =
    extendRevealed1 attrs
      $ playerLimit PerGame
      $ restricted attrs 1 Here
      $ ActionAbility []
      $ ActionCost 3

instance RunMessage VelmasDiner where
  runMessage msg l@(VelmasDiner attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> VelmasDiner <$> liftRunMessage msg attrs
