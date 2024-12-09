module Arkham.Location.Cards.ArkhamPoliceStation (arkhamPoliceStation, ArkhamPoliceStation (..)) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype ArkhamPoliceStation = ArkhamPoliceStation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamPoliceStation :: LocationCard ArkhamPoliceStation
arkhamPoliceStation = location ArkhamPoliceStation Cards.arkhamPoliceStation 3 (PerPlayer 2)

instance HasModifiersFor ArkhamPoliceStation where
  getModifiersFor (ArkhamPoliceStation a) = do
    modifySelectMap a (locationIs Cards.easttown) \lid ->
      [ConnectedToWhen (LocationWithId lid) (LocationWithId $ toId a)]

instance HasAbilities ArkhamPoliceStation where
  getAbilities (ArkhamPoliceStation attrs) = extendRevealed attrs [restrictedAbility attrs 1 Here actionAbility]

instance RunMessage ArkhamPoliceStation where
  runMessage msg l@(ArkhamPoliceStation attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      push $ search iid source iid [fromTopOfDeck 6] (basic $ withTrait Weapon) (DrawFound iid 1)
      pure l
    _ -> ArkhamPoliceStation <$> runMessage msg attrs
