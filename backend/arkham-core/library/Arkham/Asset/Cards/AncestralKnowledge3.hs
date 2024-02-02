module Arkham.Asset.Cards.AncestralKnowledge3 (
  ancestralKnowledge3,
  AncestralKnowledge3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype AncestralKnowledge3 = AncestralKnowledge3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ancestralKnowledge3 :: AssetCard AncestralKnowledge3
ancestralKnowledge3 = asset AncestralKnowledge3 Cards.ancestralKnowledge3

instance HasAbilities AncestralKnowledge3 where
  getAbilities (AncestralKnowledge3 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (DrawingStartingHand Timing.When You) Free
    , restrictedAbility attrs 2 ability2Criteria
        $ FastAbility
        $ ExhaustCost
          (toTarget attrs)
    ]
   where
    ability2Criteria =
      if null (assetCardsUnderneath attrs) then Never else ControlsThis

instance RunMessage AncestralKnowledge3 where
  runMessage msg a@(AncestralKnowledge3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      deck <- fieldMap InvestigatorDeck unDeck iid
      let
        skills =
          filter (`cardMatch` (NonWeakness <> CardWithType SkillType)) deck
      skills' <- case nonEmpty skills of
        Nothing -> error "no skills in deck"
        Just xs -> sampleN 5 xs

      push $ PlaceUnderneath (toTarget attrs) $ map PlayerCard skills'

      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      player <- getPlayer iid
      pushAll
        [ FocusCards (assetCardsUnderneath attrs)
        , chooseOne
            player
            [ targetLabel (toCardId c) [addToHand iid c]
            | c <- assetCardsUnderneath attrs
            ]
        , UnfocusCards
        ]
      pure a
    _ -> AncestralKnowledge3 <$> runMessage msg attrs
