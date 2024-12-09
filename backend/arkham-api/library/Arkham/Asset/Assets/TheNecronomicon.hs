module Arkham.Asset.Assets.TheNecronomicon (theNecronomicon) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Placement

newtype TheNecronomicon = TheNecronomicon AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomicon :: AssetCard TheNecronomicon
theNecronomicon =
  assetWith TheNecronomicon Cards.theNecronomicon
    $ (tokensL %~ setTokens #horror 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomicon where
  getModifiersFor (TheNecronomicon a) = for_ a.controller \iid -> do
    modifySelect
      a
      (ChaosTokenRevealedBy $ InvestigatorWithId iid)
      [ForcedChaosTokenChange #eldersign [#autofail]]

instance HasAbilities TheNecronomicon where
  getAbilities (TheNecronomicon a) = [controlledAbility a 1 AnyHorrorOnThis #action]

instance RunMessage TheNecronomicon where
  runMessage msg a@(TheNecronomicon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    CardEnteredPlay iid card | card.id == attrs.cardId -> do
      place attrs (InThreatArea iid)
      pure $ a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) attrs iid #horror 1
      when (attrs.horror <= 1) $ toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> TheNecronomicon <$> liftRunMessage msg attrs
