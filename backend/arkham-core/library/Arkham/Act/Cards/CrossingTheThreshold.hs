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
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype CrossingTheThreshold = CrossingTheThreshold ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crossingTheThreshold :: ActCard CrossingTheThreshold
crossingTheThreshold =
  act (1, A) CrossingTheThreshold Cards.crossingTheThreshold Nothing

instance HasAbilities CrossingTheThreshold where
  getAbilities (CrossingTheThreshold a) =
    [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ Explored Timing.After You
        $ SuccessfulExplore Anywhere
    | onSide A a
    ]

instance RunMessage CrossingTheThreshold where
  runMessage msg a@(CrossingTheThreshold attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      pushAll
        [ BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          4
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push
          $ SearchCollectionForRandom iid (toSource attrs)
          $ CardWithType PlayerTreacheryType
          <> CardWithOneOf (map CardWithTrait [Madness, Injury])
        pure a
    RequestedPlayerCard iid (isSource attrs -> True) mcard -> do
      for_ mcard $ \card -> push
        $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [PlayerCard card]
      pure a
    _ -> CrossingTheThreshold <$> runMessage msg attrs
