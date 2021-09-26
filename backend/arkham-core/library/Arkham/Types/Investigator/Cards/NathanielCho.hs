module Arkham.Types.Investigator.Cards.NathanielCho where

import Arkham.Prelude

import Arkham.Types.Investigator.Attrs
import qualified Arkham.Types.Timing as Timing
import qualified Arkham.Types.Action as Action
import Arkham.Types.Ability
import Arkham.Types.Card.CardType
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window


newtype NathanielCho = NathanielCho InvestigatorAttrs
  deriving anyclass (HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

nathanielCho :: NathanielCho
nathanielCho = NathanielCho $ baseAttrs
  "60101"
  "Nathaniel Cho"
  Guardian
  Stats
    { health = 9
    , sanity = 6
    , willpower = 3
    , intellect = 2
    , combat = 5
    , agility = 2
    }
  [Criminal, Warden]

instance HasAbilities NathanielCho where
  getAbilities (NathanielCho x) =
    [ restrictedAbility
        x
        1
        Self
        (ReactionAbility
          (EnemyDealtDamage Timing.When AnyDamageEffect AnyEnemy (SourceOwnedBy You <> SourceIsType EventType))
          Free
        )
    ]

instance HasTokenValue env NathanielCho where
  getTokenValue (NathanielCho attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance (InvestigatorRunner env) => RunMessage env NathanielCho where
  runMessage msg a@(NathanielCho attrs) = case msg of
    UseCardAbility iid source [Window _ (Window.DealtDamage _ _ (EnemyTarget eid))] 1 _ | isSource attrs source ->
      a <$ push (DirectEnemyDamage eid iid source NonAttackDamageEffect 1)
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mSource <- getSkillTestSource
      case mSource of
        Just (SkillTestSource _ _ _ _ (Just Action.Fight)) ->
          a <$ push (CreateEffect "60101" Nothing (toSource attrs) (toTarget attrs))
        _ -> pure a
    _ -> NathanielCho <$> runMessage msg attrs
