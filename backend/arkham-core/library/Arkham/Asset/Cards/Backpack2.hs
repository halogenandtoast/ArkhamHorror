module Arkham.Asset.Cards.Backpack2 (backpack2, Backpack2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), getAdditionalSearchTargets)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Strategy

newtype Backpack2 = Backpack2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backpack2 :: AssetCard Backpack2
backpack2 = asset Backpack2 Cards.backpack2

instance HasModifiersFor Backpack2 where
  getModifiersFor (InvestigatorTarget iid) (Backpack2 attrs) | controlledBy attrs iid = do
    modified attrs (AsIfInHand <$> attrs.cardsUnderneath)
  getModifiersFor _ _ = pure []

instance HasAbilities Backpack2 where
  getAbilities (Backpack2 a) =
    [restrictedAbility a 1 ControlsThis $ freeReaction $ AssetEntersPlay #after (be a)]

instance RunMessage Backpack2 where
  runMessage msg a@(Backpack2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cardMatcher = basic $ NonWeakness <> oneOf [#item, #supply]
      search iid (attrs.ability 1) iid [fromTopOfDeck 12] cardMatcher (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      chooseUpToN
        iid
        (3 + additionalTargets)
        "Done choosing cards"
        [targetLabel c [PlaceUnderneath (toTarget attrs) [c]] | c <- cards]
      pure a
    SearchNoneFound iid (isTarget attrs -> True) -> do
      chooseOne iid [Label "No Cards Found" []]
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid && card `elem` attrs.cardsUnderneath -> do
      let remaining = deleteFirstMatch (== card) attrs.cardsUnderneath
      when (null remaining) $ toDiscardBy iid attrs attrs
      addToHand iid [card]
      push msg
      pure $ Backpack2 $ attrs & cardsUnderneathL .~ remaining
    _ -> Backpack2 <$> liftRunMessage msg attrs
