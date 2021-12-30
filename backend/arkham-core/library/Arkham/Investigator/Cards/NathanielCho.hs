module Arkham.Investigator.Cards.NathanielCho where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Card.CardType
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message
import Arkham.SkillTest
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype NathanielCho = NathanielCho InvestigatorAttrs
  deriving anyclass (HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanielCho :: InvestigatorCard NathanielCho
nathanielCho = investigator
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
    [ restrictedAbility
        x
        1
        Self
        (ReactionAbility
          (EnemyDealtDamage
            Timing.When
            AnyDamageEffect
            AnyEnemy
            (SourceOwnedBy You <> SourceIsType EventType)
          )
          Free
        )
    ]

instance HasTokenValue env NathanielCho where
  getTokenValue (NathanielCho attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance (InvestigatorRunner env) => RunMessage env NathanielCho where
  runMessage msg a@(NathanielCho attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.DealtDamage _ _ (EnemyTarget eid))] 1 _
      | isSource attrs source
      -> a <$ push
        (CreateEffect "60101" Nothing (toSource attrs) (EnemyTarget eid))
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mSource <- getSkillTestSource
      case mSource of
        Just (SkillTestSource _ _ _ _ (Just Action.Fight)) ->
          a <$ push
            (CreateEffect "60101" Nothing (toSource attrs) (toTarget attrs))
        _ -> pure a
    _ -> NathanielCho <$> runMessage msg attrs
