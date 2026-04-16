module Arkham.Asset.Assets.ValeLanternAFaintHope (valeLanternAFaintHope, valeLanternAFaintHopeEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.ForMovement
import Arkham.Helpers.Location (getCanMoveTo, withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_, modified_)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Dark))
import Arkham.Window qualified as Window

newtype ValeLanternAFaintHope = ValeLanternAFaintHope AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities ValeLanternAFaintHope where
  getAbilities (ValeLanternAFaintHope a) =
    [ restricted
        a
        1
        (OnSameLocation <> exists (ConnectedFrom NotForMovement YourLocation <> UnrevealedLocation))
        $ actionAbilityWithCost (exhaust a)
    , mkAbility a 2 $ forced (AssetWouldLeavePlay #when $ AssetWithId a.id)
    ]

valeLanternAFaintHope :: AssetCard ValeLanternAFaintHope
valeLanternAFaintHope = asset ValeLanternAFaintHope Cards.valeLanternAFaintHope

instance HasModifiersFor ValeLanternAFaintHope where
  getModifiersFor (ValeLanternAFaintHope a) = do
    withLocationOf a \loc -> modified_ a loc [RemoveTrait Dark]

instance RunMessage ValeLanternAFaintHope where
  runMessage msg a@(ValeLanternAFaintHope attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select $ ConnectedFrom NotForMovement (locationWithInvestigator iid) <> UnrevealedLocation
      chooseTargetM iid locations \loc -> do
        lookAtRevealed iid (attrs.ability 1) loc
        whenM (getCanMoveTo iid (attrs.ability 1) loc) do
          chooseOneM iid $ withI18n do
            labeled' "move" do
              createCardEffect Cards.valeLanternAFaintHope (effectMetaTarget loc) attrs iid
              moveTo (attrs.ability 1) iid loc
            skip_
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      withLocationOf iid $ place attrs . AtLocation
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.valeLanternExtinguishedLight
      pure a
    _ -> ValeLanternAFaintHope <$> liftRunMessage msg attrs

newtype ValeLanternAFaintHopeEffect = ValeLanternAFaintHopeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternAFaintHopeEffect :: EffectArgs -> ValeLanternAFaintHopeEffect
valeLanternAFaintHopeEffect = cardEffect ValeLanternAFaintHopeEffect Cards.valeLanternAFaintHope

instance HasModifiersFor ValeLanternAFaintHopeEffect where
  getModifiersFor (ValeLanternAFaintHopeEffect a) = maybeModified_ a a.target do
    EffectMetaTarget (LocationTarget lid) <- hoistMaybe a.metadata
    pure
      [CannotTriggerAbilityMatching $ AbilityIsForcedAbility <> AbilityOnLocation (LocationWithId lid)]

instance RunMessage ValeLanternAFaintHopeEffect where
  runMessage msg e@(ValeLanternAFaintHopeEffect attrs) = runQueueT $ case msg of
    Do (CheckWindows ws) | any isRevealAfterWindow ws -> disableReturn e
    _ -> ValeLanternAFaintHopeEffect <$> liftRunMessage msg attrs
   where
    destLid =
      attrs.metadata >>= \case
        EffectMetaTarget (LocationTarget lid) -> Just lid
        _ -> Nothing
    isRevealAfterWindow w = case w.kind of
      Window.RevealLocation _ loc -> w.timing == #after && Just loc == destLid
      _ -> False
