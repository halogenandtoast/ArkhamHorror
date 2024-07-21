module Arkham.Act.Cards.CrossingTheThreshold (CrossingTheThreshold (..), crossingTheThreshold) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype Metadata = Metadata {advancingInvestigator :: Maybe InvestigatorId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CrossingTheThreshold = CrossingTheThreshold (ActAttrs `With` Metadata)
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crossingTheThreshold :: ActCard CrossingTheThreshold
crossingTheThreshold =
  act (1, A) (CrossingTheThreshold . (`with` Metadata Nothing)) Cards.crossingTheThreshold Nothing

instance HasAbilities CrossingTheThreshold where
  getAbilities (CrossingTheThreshold (a `With` _)) =
    [ mkAbility a 1 $ Objective $ forced $ Explored #after You $ SuccessfulExplore Anywhere | onSide A a
    ]

instance RunMessage CrossingTheThreshold where
  runMessage msg a@(CrossingTheThreshold (attrs `With` metadata)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure . CrossingTheThreshold $ attrs `with` Metadata (Just iid)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      let iid = fromJustNote "no advancing investigator" $ advancingInvestigator metadata
      sid <- getRandom
      pushAll
        [ beginSkillTest sid iid attrs iid #willpower (Fixed 4)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push
        $ SearchCollectionForRandom iid (toSource attrs)
        $ BasicWeaknessCard
        <> CardWithOneOf (map CardWithTrait [Madness, Injury])
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) (Just card) _ -> do
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card]
      pure a
    _ -> CrossingTheThreshold . (`with` metadata) <$> runMessage msg attrs
