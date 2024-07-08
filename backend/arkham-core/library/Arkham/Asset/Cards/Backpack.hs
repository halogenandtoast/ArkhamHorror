module Arkham.Asset.Cards.Backpack (backpack, Backpack (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), getAdditionalSearchTargets)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Strategy

newtype Backpack = Backpack AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backpack :: AssetCard Backpack
backpack = asset Backpack Cards.backpack

instance HasModifiersFor Backpack where
  getModifiersFor (InvestigatorTarget iid) (Backpack attrs) | controlledBy attrs iid = do
    modified attrs (AsIfInHand <$> attrs.cardsUnderneath)
  getModifiersFor _ _ = pure []

instance HasAbilities Backpack where
  getAbilities (Backpack a) =
    [restrictedAbility a 1 ControlsThis $ freeReaction $ AssetEntersPlay #after (be a)]

instance RunMessage Backpack where
  runMessage msg a@(Backpack attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let matcher = basic $ NonWeakness <> oneOf [#item, #supply]
      search iid source iid [fromTopOfDeck 6] matcher (defer attrs IsNotDraw)
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
      pure $ Backpack $ attrs & cardsUnderneathL .~ remaining
    _ -> Backpack <$> liftRunMessage msg attrs
