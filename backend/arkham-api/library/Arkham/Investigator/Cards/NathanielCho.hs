module Arkham.Investigator.Cards.NathanielCho (nathanielChoEffect, nathanielCho) where

import Arkham.Ability hiding (discardedCards)
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Helpers.Window (damagedEnemy)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype NathanielCho = NathanielCho InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

nathanielCho :: InvestigatorCard NathanielCho
nathanielCho =
  investigator NathanielCho Cards.nathanielCho
    $ Stats {health = 9, sanity = 6, willpower = 3, intellect = 2, combat = 5, agility = 2}

instance HasAbilities NathanielCho where
  getAbilities (NathanielCho x) =
    [ playerLimit PerPhase
        $ restricted x 1 Self
        $ freeReaction
        $ EnemyDealtDamage #when AnyDamageEffect AnyEnemy (SourceOwnedBy You <> SourceIsType EventType)
    ]

instance HasChaosTokenValue NathanielCho where
  getChaosTokenValue iid ElderSign (NathanielCho attrs) | iid == investigatorId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage NathanielCho where
  runMessage msg a@(NathanielCho attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (damagedEnemy -> eid) _ -> do
      createCardEffect Cards.nathanielCho Nothing attrs eid
      pure a
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mAction <- getSkillTestAction
      when (mAction == Just #fight) do
        createCardEffect Cards.nathanielCho Nothing attrs attrs
      pure a
    _ -> NathanielCho <$> liftRunMessage msg attrs

newtype NathanielChoEffect = NathanielChoEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

nathanielChoEffect :: EffectArgs -> NathanielChoEffect
nathanielChoEffect = cardEffect NathanielChoEffect Cards.nathanielCho

instance HasModifiersFor NathanielChoEffect where
  getModifiersFor (NathanielChoEffect a) = modified_ a a.target [DamageTaken 1]

isTakeDamage :: EffectAttrs -> Window -> Bool
isTakeDamage attrs window = case attrs.target of
  EnemyTarget eid -> go eid
  _ -> False
 where
  go eid = case windowType window of
    Window.TakeDamage _ _ (EnemyTarget eid') _ ->
      eid == eid' && windowTiming window == #after
    _ -> False

instance RunMessage NathanielChoEffect where
  runMessage msg e@(NathanielChoEffect attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (Initiator {}) _ _ | attrs.target == InvestigatorTarget iid -> do
      discardedCards <- field InvestigatorDiscard iid
      let events = filter ((== EventType) . toCardType) discardedCards
      if null events
        then disable attrs
        else focusCards (map toCard events) do
          chooseOneM iid do
            questionLabeled "{elderSign}: return an event from your discard pile to your hand."
            targets events (returnToHand iid)
          disable attrs
      pure e
    Do (CheckWindows windows') | any (isTakeDamage attrs) windows' -> disableReturn e
    SkillTestEnds {} -> disableReturn e
    _ -> NathanielChoEffect <$> liftRunMessage msg attrs
