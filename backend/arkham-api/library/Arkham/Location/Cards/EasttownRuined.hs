module Arkham.Location.Cards.EasttownRuined (easttownRuined) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Helpers (decreaseFloodLevel)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Ally))

newtype EasttownRuined = EasttownRuined LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttownRuined :: LocationCard EasttownRuined
easttownRuined = location EasttownRuined Cards.easttownRuined 3 (Static 1)

instance HasModifiersFor EasttownRuined where
  getModifiersFor (EasttownRuined a) = modifySelf a [CannotBeFullyFlooded]

instance HasAbilities EasttownRuined where
  getAbilities (EasttownRuined a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (DiscardAssetCost $ AssetWithTrait Ally <> AssetControlledBy You)

instance RunMessage EasttownRuined where
  runMessage msg l@(EasttownRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      decreaseFloodLevel attrs.id
      drawCards iid (attrs.ability 1) 3
      pure l
    _ -> EasttownRuined <$> liftRunMessage msg attrs
