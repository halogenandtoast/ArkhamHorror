module Arkham.Investigator.Cards.NathanielCho (NathanielCho, nathanielChoEffect, nathanielCho) where

import Arkham.Ability hiding (discardedCards)
import Arkham.Card
import Arkham.Effect.Runner
import Arkham.Helpers.Effect
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner hiding (discardedCards)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude
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
        $ restrictedAbility x 1 Self
        $ freeReaction
        $ EnemyDealtDamage #when AnyDamageEffect AnyEnemy (SourceOwnedBy You <> SourceIsType EventType)
    ]

instance HasChaosTokenValue NathanielCho where
  getChaosTokenValue iid ElderSign (NathanielCho attrs) | iid == investigatorId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getEnemyId :: [Window] -> EnemyId
getEnemyId = \case
  ((windowType -> Window.DealtDamage _ _ (EnemyTarget eid) _) : _) -> eid
  _ -> error "Expected DealtDamage window"

instance RunMessage NathanielCho where
  runMessage msg a@(NathanielCho attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemyId -> eid) _ -> do
      push $ createCardEffect Cards.nathanielCho Nothing attrs eid
      pure a
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mAction <- getSkillTestAction
      pushWhen (mAction == Just #fight)
        $ createCardEffect Cards.nathanielCho Nothing attrs attrs
      pure a
    _ -> NathanielCho <$> runMessage msg attrs

newtype NathanielChoEffect = NathanielChoEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

nathanielChoEffect :: EffectArgs -> NathanielChoEffect
nathanielChoEffect = cardEffect NathanielChoEffect Cards.nathanielCho

instance HasModifiersFor NathanielChoEffect where
  getModifiersFor target@(EnemyTarget _) (NathanielChoEffect attrs) | attrs.target == target = do
    pure $ toModifiers attrs [DamageTaken 1]
  getModifiersFor _ _ = pure []

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
  runMessage msg e@(NathanielChoEffect attrs) = case msg of
    PassedSkillTest iid _ _ (Initiator {}) _ _ | attrs.target == InvestigatorTarget iid -> do
      discardedCards <- field InvestigatorDiscard iid
      let events = filter ((== EventType) . toCardType) discardedCards
      if null events
        then push $ disable attrs
        else do
          player <- getPlayer iid
          pushAll
            [ FocusCards (map toCard events)
            , questionLabel "{elderSign}: return an event from your discard pile to your hand." player
                $ ChooseOne
                  [ targetLabel (toCardId event) [UnfocusCards, ReturnToHand iid (CardTarget $ PlayerCard event)]
                  | event <- events
                  ]
            , disable attrs
            ]
      pure e
    CheckWindow _ windows' | any (isTakeDamage attrs) windows' -> do
      push $ disable attrs
      pure e
    SkillTestEnds _ _ _ -> do
      push $ disable attrs
      pure e
    _ -> NathanielChoEffect <$> runMessage msg attrs
