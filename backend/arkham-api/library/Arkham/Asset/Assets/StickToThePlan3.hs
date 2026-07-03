module Arkham.Asset.Assets.StickToThePlan3 (stickToThePlan3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Investigator (getCardAttachments)
import Arkham.Helpers.Modifiers hiding (cardResolutionModifier, costModifier)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
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
    controllerGets a (map (AsIfInHandFor ForPlay . toCardId) a.cardsUnderneath)
    modifyEach a a.cardsUnderneath [AdditionalCost $ exhaust a]

instance HasAbilities StickToThePlan3 where
  getAbilities (StickToThePlan3 attrs) = [controlled_ attrs 1 $ freeReaction $ DrawingStartingHand #when You]

pickCardsByCodes :: [CardCode] -> [Card] -> Maybe [Card]
pickCardsByCodes [] _ = Just []
pickCardsByCodes (code : codes) cards = do
  let (before, rest) = break ((== code) . (.cardCode)) cards
  case rest of
    [] -> Nothing
    card : after -> (card :) <$> pickCardsByCodes codes (before <> after)

instance RunMessage StickToThePlan3 where
  runMessage msg a@(StickToThePlan3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      attachments <- getCardAttachments iid attrs
      if null attachments
        then search iid attrs iid [fromDeck] #any (defer attrs IsNotDraw)
        else do
          deck <- fieldMap InvestigatorDeck (map toCard . (.cards)) iid
          let
            tacticsAndSupplies =
              nubBy ((==) `on` toCardCode)
                $ filterCards (#event <> mapOneOf CardWithTrait [Trait.Tactic, Trait.Supply]) deck
          case pickCardsByCodes attachments tacticsAndSupplies of
            Just selected | length selected <= 3 -> do
              traverse_ obtainCard selected
              placeUnderneath attrs selected
            _ -> search iid attrs iid [fromDeck] #any (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let
        tacticsAndSupplies =
          nubBy ((==) `on` toCardCode)
            $ filterCards (#event <> mapOneOf CardWithTrait [Trait.Tactic, Trait.Supply]) cards
      if null tacticsAndSupplies
        then withI18n $ prompt_ iid "noCardsFound"
        else getTotalSearchTargets iid tacticsAndSupplies 3 >>= (`doStep` msg)
      pure a
    DoStep n (SearchFound iid t@(isTarget attrs -> True) deck cards) | notNull cards && n > 0 -> do
      let
        tacticsAndSupplies =
          nubBy ((==) `on` toCardCode)
            $ filterCards (#event <> mapOneOf CardWithTrait [Trait.Tactic, Trait.Supply]) cards

      cardI18n $ scope "stickToThePlan3" $ chooseUpToNM' iid 1 "chooseNoMoreEvents" do
        targets tacticsAndSupplies \card -> do
          let remaining = filterCards (not_ (CardWithId card.id) <> not_ (CardWithTitle card.title)) tacticsAndSupplies
          push $ RemoveCardFromSearch iid (toCardId card)
          placeUnderneath attrs [card]
          doStep (n - 1) $ SearchFound iid t deck remaining
      pure a
    _ -> StickToThePlan3 <$> liftRunMessage msg attrs
