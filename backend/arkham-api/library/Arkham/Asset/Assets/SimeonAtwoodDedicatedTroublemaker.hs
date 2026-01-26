module Arkham.Asset.Assets.SimeonAtwoodDedicatedTroublemaker (simeonAtwoodDedicatedTroublemaker) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)

newtype SimeonAtwoodDedicatedTroublemaker = SimeonAtwoodDedicatedTroublemaker AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

simeonAtwoodDedicatedTroublemaker :: AssetCard SimeonAtwoodDedicatedTroublemaker
simeonAtwoodDedicatedTroublemaker = asset SimeonAtwoodDedicatedTroublemaker Cards.simeonAtwoodDedicatedTroublemaker

instance HasAbilities SimeonAtwoodDedicatedTroublemaker where
  getAbilities (SimeonAtwoodDedicatedTroublemaker a) =
    [groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage SimeonAtwoodDedicatedTroublemaker where
  runMessage msg a@(SimeonAtwoodDedicatedTroublemaker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 3
      pure a
    _ -> SimeonAtwoodDedicatedTroublemaker <$> liftRunMessage msg attrs
