module Arkham.Asset.Cards.RiteOfSeeking2 (riteOfSeeking2, RiteOfSeeking2 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Modifier

newtype RiteOfSeeking2 = RiteOfSeeking2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking2 :: AssetCard RiteOfSeeking2
riteOfSeeking2 = asset RiteOfSeeking2 Cards.riteOfSeeking2

instance HasAbilities RiteOfSeeking2 where
  getAbilities (RiteOfSeeking2 a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage RiteOfSeeking2 where
  runMessage msg a@(RiteOfSeeking2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)

      createCardEffect Cards.riteOfSeeking (effectMetaTarget sid) source (InvestigationTarget iid lid) -- same effect as base
      skillTestModifiers sid source iid [SkillModifier #willpower 2, DiscoveredClues 1]
      pushAll $ leftOr investigation
      pure a
    _ -> RiteOfSeeking2 <$> liftRunMessage msg attrs
