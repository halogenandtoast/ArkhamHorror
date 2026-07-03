module Arkham.Asset.Assets.AncestralKnowledge3 (ancestralKnowledge3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Card.CardCode
import Arkham.Helpers.Investigator (getCardAttachments)
import Arkham.Investigator.Projection ()
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype AncestralKnowledge3 = AncestralKnowledge3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancestralKnowledge3 :: AssetCard AncestralKnowledge3
ancestralKnowledge3 = asset AncestralKnowledge3 Cards.ancestralKnowledge3

instance HasAbilities AncestralKnowledge3 where
  getAbilities (AncestralKnowledge3 a) =
    [ restricted a 1 ControlsThis $ freeReaction $ DrawingStartingHand #when You
    , restricted a 2 ability2Criteria $ FastAbility $ exhaust a
    ]
   where
    ability2Criteria = if null a.cardsUnderneath then Never else ControlsThis

pickCardsByCodes :: [CardCode] -> [Card] -> Maybe [Card]
pickCardsByCodes [] _ = Just []
pickCardsByCodes (code : codes) cards = do
  let (before, rest) = break ((== code) . (.cardCode)) cards
  case rest of
    [] -> Nothing
    card : after -> (card :) <$> pickCardsByCodes codes (before <> after)

instance RunMessage AncestralKnowledge3 where
  runMessage msg a@(AncestralKnowledge3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      attachments <- getCardAttachments iid attrs
      if null attachments
        then do
          deck <- iid.deck
          skills <- case nonEmpty (filterCards (NonWeakness <> #skill) deck) of
            Nothing -> error "no skills in deck"
            Just xs -> sampleN 5 xs
          placeUnderneath attrs skills
        else do
          deck <- fieldMap InvestigatorDeck (map toCard . (.cards)) iid
          let skills = filterCards (NonWeakness <> #skill) deck
          case pickCardsByCodes attachments skills of
            Just selected | length selected <= 5 -> do
              traverse_ obtainCard selected
              placeUnderneath attrs selected
            _ -> do
              skills' <- case nonEmpty skills of
                Nothing -> error "no skills in deck"
                Just xs -> sampleN 5 xs
              placeUnderneath attrs skills'
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      focusCards attrs.cardsUnderneath do
        chooseTargetM iid attrs.cardsUnderneath (drawToHand iid . only)
      pure a
    _ -> AncestralKnowledge3 <$> liftRunMessage msg attrs
