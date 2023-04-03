module Arkham.Investigator.Cards.WinifredHabbamock
  ( winifredHabbamock
  , winifredHabbamockEffect
  , WinifredHabbamock(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.Source

newtype WinifredHabbamock = WinifredHabbamock InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winifredHabbamock :: InvestigatorCard WinifredHabbamock
winifredHabbamock = investigator
  WinifredHabbamock
  Cards.winifredHabbamock
  Stats
    { health = 8
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 3
    , agility = 5
    }

instance HasAbilities WinifredHabbamock where
  getAbilities (WinifredHabbamock a) =
    [ limitedAbility (PlayerLimit PerTestOrAbility 1)
      $ restrictedAbility a 1 (Self <> CommitedCardsMatch (DifferentLengthIsAtLeast 2 (NonWeakness <> CardOwnedBy (toId a))))
      $ FastAbility Free
    ]

instance HasTokenValue WinifredHabbamock where
  getTokenValue iid ElderSign (WinifredHabbamock attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage WinifredHabbamock where
  runMessage msg i@(WinifredHabbamock attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawing <- drawCards iid (toAbilitySource attrs 1) 1
      push drawing
      pure i
    ResolveToken _ ElderSign iid | iid == toId attrs -> do
      push $ createCardEffect Cards.winifredHabbamock Nothing attrs attrs
      pure i
    _ -> WinifredHabbamock <$> runMessage msg attrs

newtype WinifredHabbamockEffect = WinifredHabbamockEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winifredHabbamockEffect :: EffectArgs -> WinifredHabbamockEffect
winifredHabbamockEffect = cardEffect WinifredHabbamockEffect Cards.winifredHabbamock

instance RunMessage WinifredHabbamockEffect where
  runMessage msg e@(WinifredHabbamockEffect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ -> do
      push $ DisableEffect (toId e)
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> error "no skill test"
        Just st -> case skillTestResult st of
          SucceededBy _ n -> case effectSource of
            InvestigatorSource iid -> do
              committedCards <- field InvestigatorCommittedCards iid
              unless (null committedCards) $ do
                push $ chooseN iid (min (n `div` 2) (length committedCards)) [targetLabel (toCardId card) [ReturnToHand iid (toTarget $ toCardId card)] | card <- committedCards]
            _ -> error "invalid source"
          _ -> pure ()
      pure e
    _ -> WinifredHabbamockEffect <$> runMessage msg attrs
