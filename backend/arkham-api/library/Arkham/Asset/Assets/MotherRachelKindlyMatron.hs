module Arkham.Asset.Assets.MotherRachelKindlyMatron (motherRachelKindlyMatron) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Resident (..), codex, decreaseRelationshipLevel)

newtype MotherRachelKindlyMatron = MotherRachelKindlyMatron AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

motherRachelKindlyMatron :: AssetCard MotherRachelKindlyMatron
motherRachelKindlyMatron = assetWith MotherRachelKindlyMatron Cards.motherRachelKindlyMatron $ (healthL ?~ 4) . (sanityL ?~ 4)

instance HasModifiersFor MotherRachelKindlyMatron where
  getModifiersFor (MotherRachelKindlyMatron a) = controllerGets a [AdditionalSlot #arcane]

instance HasAbilities MotherRachelKindlyMatron where
  getAbilities (MotherRachelKindlyMatron a) =
    [ mkAbility a 99 $ forced $ AssetDefeated #when ByAny (be a)
    ,groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage MotherRachelKindlyMatron where
  runMessage msg a@(MotherRachelKindlyMatron attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 99 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      push $ SetAsideCards [toCard attrs]
      decreaseRelationshipLevel MotherRachel 1
      pure a
    _ -> MotherRachelKindlyMatron <$> liftRunMessage msg attrs
