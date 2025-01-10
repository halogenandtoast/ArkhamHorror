module Arkham.Asset.Assets.AncestralKnowledge3 (ancestralKnowledge3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Investigator.Projection ()
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose

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

instance RunMessage AncestralKnowledge3 where
  runMessage msg a@(AncestralKnowledge3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      deck <- iid.deck
      skills <- case nonEmpty (filterCards (NonWeakness <> #skill) deck) of
        Nothing -> error "no skills in deck"
        Just xs -> sampleN 5 xs
      placeUnderneath attrs skills
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      focusCards attrs.cardsUnderneath do
        chooseTargetM iid attrs.cardsUnderneath (addToHand iid . only)
      pure a
    _ -> AncestralKnowledge3 <$> liftRunMessage msg attrs
