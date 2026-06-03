module Arkham.Treachery.Cards.FungalRot (fungalRot) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Item))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FungalRot = FungalRot TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fungalRot :: TreacheryCard FungalRot
fungalRot = treachery FungalRot Cards.fungalRot

instance HasModifiersFor FungalRot where
  getModifiersFor (FungalRot a) = case a.placement of
    AttachedToAsset aid _ -> modified_ a aid [Blank]
    _ -> pure ()

instance HasAbilities FungalRot where
  getAbilities (FungalRot a) =
    [skillTestAbility $ restricted a 1 OnSameLocation actionAbility]

instance RunMessage FungalRot where
  runMessage msg t@(FungalRot attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <-
        select
          $ assetControlledBy iid
          <> AssetWithTrait Item
          <> not_ (AssetWithAttachedTreachery $ treacheryIs Cards.fungalRot)
      if null assets
        then gainSurge attrs
        else chooseTargetM iid assets $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    _ -> FungalRot <$> liftRunMessage msg attrs
