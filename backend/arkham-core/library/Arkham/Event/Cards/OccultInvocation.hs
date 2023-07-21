module Arkham.Event.Cards.OccultInvocation (
  occultInvocation,
  OccultInvocation (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.SkillType

newtype OccultInvocation = OccultInvocation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultInvocation :: EventCard OccultInvocation
occultInvocation = event OccultInvocation Cards.occultInvocation

countCards :: Payment -> Int
countCards = \case
  DiscardCardPayment xs -> length xs
  Payments ps -> sum $ map countCards ps
  _ -> 0

instance RunMessage OccultInvocation where
  runMessage msg e@(OccultInvocation attrs) = case msg of
    PaidForCardCost iid card (countCards -> n) | toCardId card == toCardId attrs -> do
      pushAll
        [ skillTestModifiers
            (toSource attrs)
            (InvestigatorTarget iid)
            [DamageDealt n, SkillModifier SkillIntellect n]
        , ChooseFightEnemy
            iid
            (toSource attrs)
            Nothing
            SkillIntellect
            mempty
            False
        ]
      pure e
    _ -> OccultInvocation <$> runMessage msg attrs
