module Arkham.Location.Cards.AncientCanyons (ancientCanyons) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card (toCard)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Monster))

newtype AncientCanyons = AncientCanyons LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientCanyons :: LocationCard AncientCanyons
ancientCanyons = location AncientCanyons Cards.ancientCanyons 3 (Static 1)

instance HasAbilities AncientCanyons where
  getAbilities (AncientCanyons a) =
    extendRevealed
      a
      [ skillTestAbility $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , restricted a 2 Here actionAbility
      ]

instance RunMessage AncientCanyons where
  runMessage msg l@(AncientCanyons attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 1)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      findEncounterCardIn iid attrs (#enemy <> CardWithTrait Monster) [FromEncounterDeck, FromEncounterDiscard]
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      withLocationOf iid \lid ->
        push $ SpawnEnemyAtEngagedWith (toCard card) lid iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.skyRelic
      pure l
    _ -> AncientCanyons <$> liftRunMessage msg attrs
