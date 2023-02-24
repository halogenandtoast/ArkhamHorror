module Arkham.Act.Cards.CrossingTheThreshold
  ( CrossingTheThreshold(..)
  , crossingTheThreshold
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype Metadata = Metadata { advancingInvestigator :: Maybe InvestigatorId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CrossingTheThreshold = CrossingTheThreshold (ActAttrs `With` Metadata)
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crossingTheThreshold :: ActCard CrossingTheThreshold
crossingTheThreshold = act
  (1, A)
  (CrossingTheThreshold . (`with` Metadata Nothing))
  Cards.crossingTheThreshold
  Nothing

instance HasAbilities CrossingTheThreshold where
  getAbilities (CrossingTheThreshold (a `With` _)) =
    [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ Explored Timing.After You
        $ SuccessfulExplore Anywhere
    | onSide A a
    ]

instance RunMessage CrossingTheThreshold where
  runMessage msg a@(CrossingTheThreshold (attrs `With` metadata)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure . CrossingTheThreshold $ attrs `with` Metadata (Just iid)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      let
        iid = fromJustNote "no advancing investigator"
          $ advancingInvestigator metadata
      pushAll
        [ beginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          SkillWillpower
          4
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push
          $ SearchCollectionForRandom iid (toSource attrs)
          $ PlayerTreachery
          <> CardWithOneOf (map CardWithTrait [Madness, Injury])
        pure a
    RequestedPlayerCard iid (isSource attrs -> True) (Just card) -> do
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card]
      pure a
    _ -> CrossingTheThreshold . (`with` metadata) <$> runMessage msg attrs
