module Arkham.Asset.Assets.Backpack2 (backpack2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.Search
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Strategy

newtype Backpack2 = Backpack2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backpack2 :: AssetCard Backpack2
backpack2 = asset Backpack2 Cards.backpack2

instance HasModifiersFor Backpack2 where
  getModifiersFor (Backpack2 a) = controllerGets a (AsIfInHand <$> a.cardsUnderneath)

instance HasAbilities Backpack2 where
  getAbilities (Backpack2 a) = [restricted a 1 ControlsThis $ freeReaction $ AssetEntersPlay #after (be a)]

instance RunMessage Backpack2 where
  runMessage msg a@(Backpack2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cardMatcher = basic $ NonWeakness <> oneOf [#item, #supply]
      search iid (attrs.ability 1) iid [fromTopOfDeck 12] cardMatcher (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      chooseFromSearch iid 3 cards \card -> placeUnderneath attrs [card]
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid && card `under` attrs -> do
      let remaining = deleteFirst card attrs.cardsUnderneath
      when (null remaining) $ toDiscardBy iid attrs attrs
      costModifier attrs iid (AsIfInHandForPlay card.id)
      push msg
      pure $ Backpack2 $ attrs & cardsUnderneathL .~ remaining
    ResolvedCard _ c | c.id == attrs.cardId -> do
      when (null attrs.cardsUnderneath) $ toDiscard attrs attrs
      pure a
    _ -> do
      let hadCards = notNull $ attrs.cardsUnderneath
      result <- liftRunMessage msg attrs
      when (hadCards && null result.cardsUnderneath) $ toDiscard attrs attrs
      pure $ Backpack2 result
