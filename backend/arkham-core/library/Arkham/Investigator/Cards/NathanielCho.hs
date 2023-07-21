module Arkham.Investigator.Cards.NathanielCho where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card.CardType
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message
import Arkham.SkillTest
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype NathanielCho = NathanielCho InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanielCho :: InvestigatorCard NathanielCho
nathanielCho =
  investigator
    NathanielCho
    Cards.nathanielCho
    Stats
      { health = 9
      , sanity = 6
      , willpower = 3
      , intellect = 2
      , combat = 5
      , agility = 2
      }

instance HasAbilities NathanielCho where
  getAbilities (NathanielCho x) =
    [ restrictedAbility x 1 Self $
        ReactionAbility
          ( EnemyDealtDamage
              Timing.When
              AnyDamageEffect
              AnyEnemy
              (SourceOwnedBy You <> SourceIsType EventType)
          )
          Free
    ]

instance HasChaosTokenValue NathanielCho where
  getChaosTokenValue iid ElderSign (NathanielCho attrs)
    | iid == investigatorId attrs =
        pure $
          ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage NathanielCho where
  runMessage msg a@(NathanielCho attrs) = case msg of
    UseCardAbility _ source 1 [Window _ (Window.DealtDamage _ _ (EnemyTarget eid) _)] _
      | isSource attrs source ->
          a
            <$ push
              (CreateEffect "60101" Nothing (toSource attrs) (EnemyTarget eid))
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mSource <- getSkillTestSource
      case mSource of
        Just (SkillTestSource _ _ _ (Just Action.Fight)) ->
          a
            <$ push
              (CreateEffect "60101" Nothing (toSource attrs) (toTarget attrs))
        _ -> pure a
    _ -> NathanielCho <$> runMessage msg attrs
