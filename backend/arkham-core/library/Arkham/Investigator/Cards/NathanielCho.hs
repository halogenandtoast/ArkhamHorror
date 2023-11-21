module Arkham.Investigator.Cards.NathanielCho where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card.CardType
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.SkillTest
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype NathanielCho = NathanielCho InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanielCho :: InvestigatorCard NathanielCho
nathanielCho =
  investigator NathanielCho Cards.nathanielCho
    $ Stats {health = 9, sanity = 6, willpower = 3, intellect = 2, combat = 5, agility = 2}

instance HasAbilities NathanielCho where
  getAbilities (NathanielCho x) =
    [ playerLimit PerPhase
        $ restrictedAbility x 1 Self
        $ ReactionAbility
          ( EnemyDealtDamage Timing.When AnyDamageEffect AnyEnemy (SourceOwnedBy You <> SourceIsType EventType)
          )
          Free
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
      push $ CreateEffect "60101" Nothing (toSource attrs) (EnemyTarget eid)
      pure a
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mAction <- getSkillTestAction
      pushWhen (mAction == Just Action.Fight)
        $ CreateEffect "60101" Nothing (toSource attrs) (toTarget attrs)
      pure a
    _ -> NathanielCho <$> runMessage msg attrs
