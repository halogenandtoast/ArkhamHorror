module Arkham.Investigator.Cards.WinifredHabbamock (winifredHabbamock, winifredHabbamockEffect) where

import Arkham.Ability
import Arkham.Effect.Import
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorCommittedCards))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.SkillTest.Base
import Arkham.SkillTestResult

newtype WinifredHabbamock = WinifredHabbamock InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

winifredHabbamock :: InvestigatorCard WinifredHabbamock
winifredHabbamock =
  investigator WinifredHabbamock Cards.winifredHabbamock
    $ Stats {health = 8, sanity = 7, willpower = 1, intellect = 3, combat = 3, agility = 5}

instance HasAbilities WinifredHabbamock where
  getAbilities (WinifredHabbamock a) =
    [ wantsSkillTest AnySkillTest
        $ playerLimit PerTestOrAbility
        $ restricted
          a
          1
          (Self <> CommitedCardsMatch (DifferentLengthIsAtLeast 2 (NonWeakness <> CardOwnedBy a.id)))
          (FastAbility Free)
    ]

instance HasChaosTokenValue WinifredHabbamock where
  getChaosTokenValue iid ElderSign (WinifredHabbamock attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage WinifredHabbamock where
  runMessage msg i@(WinifredHabbamock attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure i
    ResolveChaosToken _ ElderSign iid | iid == toId attrs -> do
      createCardEffect Cards.winifredHabbamock Nothing attrs attrs
      pure i
    _ -> WinifredHabbamock <$> liftRunMessage msg attrs

newtype WinifredHabbamockEffect = WinifredHabbamockEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

winifredHabbamockEffect :: EffectArgs -> WinifredHabbamockEffect
winifredHabbamockEffect = cardEffect WinifredHabbamockEffect Cards.winifredHabbamock

instance RunMessage WinifredHabbamockEffect where
  runMessage msg e@(WinifredHabbamockEffect attrs) = runQueueT $ case msg of
    SkillTestEnds {} -> do
      getSkillTest >>= traverse_ \st -> case skillTestResult st of
        SucceededBy _ n -> case attrs.source of
          InvestigatorSource iid -> do
            cards <- field InvestigatorCommittedCards iid
            when (notNull cards) do
              afterSkillTest iid "Winifred Habbamock" do
                focusCards cards do
                  chooseNM iid (min (n `div` 2) (length cards)) do
                    targets cards (returnToHand iid)
          _ -> error "invalid source"
        _ -> pure ()
      disableReturn e
    _ -> WinifredHabbamockEffect <$> liftRunMessage msg attrs
