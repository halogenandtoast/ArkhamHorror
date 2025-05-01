module Arkham.Asset.Assets.StickToThePlan3 (stickToThePlan3) where

import Arkham.I18n
import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers hiding (costModifier)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait qualified as Trait
import Data.Function (on)
import Data.List (nubBy)

newtype StickToThePlan3 = StickToThePlan3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickToThePlan3 :: AssetCard StickToThePlan3
stickToThePlan3 = asset StickToThePlan3 Cards.stickToThePlan3

instance HasModifiersFor StickToThePlan3 where
  getModifiersFor (StickToThePlan3 a) = do
    controllerGets a (map AsIfInHand $ assetCardsUnderneath a)
    modifyEach a (assetCardsUnderneath a) [AdditionalCost $ exhaust a]

instance HasAbilities StickToThePlan3 where
  getAbilities (StickToThePlan3 attrs) =
    [restricted attrs 1 ControlsThis $ freeReaction $ DrawingStartingHand #when You]

instance RunMessage StickToThePlan3 where
  runMessage msg a@(StickToThePlan3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid attrs iid [fromDeck] #any (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let
        tacticsAndSupplies =
          nubBy ((==) `on` toCardCode)
            $ filterCards (#event <> mapOneOf CardWithTrait [Trait.Tactic, Trait.Supply]) cards
      focusCards cards do
        if null tacticsAndSupplies
          then withI18n $ prompt_ iid "noCardsFound"
          else do
            totalTargets <- getTotalSearchTargets iid tacticsAndSupplies 3
            chooseUpToNM iid totalTargets "Choose no more events" do
              targets tacticsAndSupplies \card -> do
                push $ RemoveCardFromSearch iid (toCardId card)
                placeUnderneath attrs [card]
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid && card `elem` assetCardsUnderneath attrs -> do
      let remaining = deleteFirstMatch (== card) $ assetCardsUnderneath attrs
      costModifier attrs (toCardId card) (AdditionalCost $ ExhaustCost $ toTarget attrs)
      push msg
      pure $ StickToThePlan3 $ attrs & cardsUnderneathL .~ remaining
    _ -> StickToThePlan3 <$> liftRunMessage msg attrs
