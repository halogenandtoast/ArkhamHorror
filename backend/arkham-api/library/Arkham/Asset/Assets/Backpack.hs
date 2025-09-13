module Arkham.Asset.Assets.Backpack (backpack) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.Search
import Arkham.Matcher hiding (PlaceUnderneath, PlayCard)
import Arkham.Strategy

newtype Backpack = Backpack AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backpack :: AssetCard Backpack
backpack = asset Backpack Cards.backpack

instance HasModifiersFor Backpack where
  getModifiersFor (Backpack a) = controllerGets a (AsIfInHand <$> a.cardsUnderneath)

instance HasAbilities Backpack where
  getAbilities (Backpack a) = [restricted a 1 ControlsThis $ freeReaction $ AssetEntersPlay #after (be a)]

instance RunMessage Backpack where
  runMessage msg a@(Backpack attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let matcher = basic $ NonWeakness <> oneOf [#item, #supply]
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] matcher (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      chooseFromSearch iid 3 cards \card -> placeUnderneath attrs [card]
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid && card `under` attrs -> do
      let remaining = deleteFirst card attrs.cardsUnderneath
      when (null remaining) $ toDiscardBy iid attrs attrs
      costModifier attrs iid (AsIfInHandForPlay card.id)
      push msg
      pure $ Backpack $ attrs & cardsUnderneathL .~ remaining
    ResolvedCard _ c | c.id == attrs.cardId -> do
      when (null attrs.cardsUnderneath) $ toDiscard attrs attrs
      pure a
    _ -> do
      let hadCards = notNull attrs.cardsUnderneath
      result <- liftRunMessage msg attrs
      when (hadCards && null result.cardsUnderneath) $ toDiscard attrs attrs
      pure $ Backpack result
