module Arkham.Asset.Assets.TheNecronomiconDrakeTranslation (theNecronomiconDrakeTranslation) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype TheNecronomiconDrakeTranslation = TheNecronomiconDrakeTranslation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconDrakeTranslation :: AssetCard TheNecronomiconDrakeTranslation
theNecronomiconDrakeTranslation = assetWith TheNecronomiconDrakeTranslation Cards.theNecronomiconDrakeTranslation cannotLeavePlay

instance HasModifiersFor TheNecronomiconDrakeTranslation where
  getModifiersFor (TheNecronomiconDrakeTranslation a) = for_ a.controller \iid -> do
    modified_
      a
      iid
      [ CannotPlay #asset
      , CannotTriggerAbilityMatching (AbilityOnAsset $ assetControlledBy iid <> not_ (AssetWithId a.id))
      ]

instance HasAbilities TheNecronomiconDrakeTranslation where
  getAbilities (TheNecronomiconDrakeTranslation a) =
    [ skillTestAbility $ controlled_ a 1 actionAbility
    ]

instance RunMessage TheNecronomiconDrakeTranslation where
  runMessage msg a@(TheNecronomiconDrakeTranslation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      place attrs (InThreatArea iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 5)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      shuffleIntoDeck iid attrs
      assignHorror iid (attrs.ability 1) 1
      pure a
    _ -> TheNecronomiconDrakeTranslation <$> liftRunMessage msg attrs
