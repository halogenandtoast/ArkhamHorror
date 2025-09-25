module Arkham.Treachery.Cards.ViceAndVillainy (viceAndVillainy) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ViceAndVillainy = ViceAndVillainy TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viceAndVillainy :: TreacheryCard ViceAndVillainy
viceAndVillainy = treachery ViceAndVillainy Cards.viceAndVillainy

instance HasAbilities ViceAndVillainy where
  getAbilities (ViceAndVillainy a) = case a.attached.asset of
    Nothing -> []
    Just aid ->
      [ restricted a 1 (youExist $ HasMatchingAsset $ AssetWithId aid <> DiscardableAsset)
          $ forced EncounterDeckRunsOutOfCards
      , skillTestAbility
          $ mkAbility a 2
          $ forced
          $ AssetLeavesPlay #when (AssetControlledBy You <> AssetWithId aid)
      ]

instance RunMessage ViceAndVillainy where
  runMessage msg t@(ViceAndVillainy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <-
        select
          $ assetControlledBy iid
          <> not_ PermanentAsset
          <> not_ (AssetWithAttachedTreachery $ treacheryIs Cards.viceAndVillainy)
      chooseTargetM iid assets $ placeTreachery attrs . (`AttachedToAsset` Nothing)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.asset $ toDiscardBy iid (attrs.ability 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      for_ attrs.attached.asset \aid -> do
        assets <-
          select
            $ assetControlledBy iid
            <> not_ (AssetWithId aid)
            <> not_ PermanentAsset
            <> not_ (AssetWithAttachedTreachery $ treacheryIs Cards.viceAndVillainy)
        chooseTargetM iid assets $ placeTreachery attrs . (`AttachedToAsset` Nothing)
      pure t
    _ -> ViceAndVillainy <$> liftRunMessage msg attrs
