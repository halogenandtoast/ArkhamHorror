module Arkham.Location.Cards.RailBridge (railBridge) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token

newtype RailBridge = RailBridge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

railBridge :: LocationCard RailBridge
railBridge = location RailBridge Cards.railBridge 0 (PerPlayer 2)

instance HasModifiersFor RailBridge where
  getModifiersFor (RailBridge a) = for_ a.position \(Pos x _) -> modifySelf a [ShroudModifier x]

instance HasAbilities RailBridge where
  getAbilities (RailBridge a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ actionAbilityWithCost (GroupResourceCost (PerPlayer 1) Anywhere)

instance RunMessage RailBridge where
  runMessage msg l@(RailBridge attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) ScenarioTarget Switch 1
      pure l
    _ -> RailBridge <$> liftRunMessage msg attrs
