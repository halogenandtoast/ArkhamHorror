module Arkham.Event.Cards.Marksmanship2
  ( marksmanship2
  , marksmanship2Effect
  , Marksmanship2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher hiding (EventCard)
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window

newtype Marksmanship2 = Marksmanship2 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship2 :: EventCard Marksmanship2
marksmanship2 = event Marksmanship2 Cards.marksmanship2

instance HasModifiersFor Marksmanship2 where
  getModifiersFor (AbilityTarget iid ability) (Marksmanship2 a)
    | eventOwner a == iid = do
      case abilityAction ability of
        Just Action.Fight -> do
          traits <- sourceTraits (abilitySource ability)
          if any (`elem` traits) [Firearm, Ranged]
            then do
              isPlayable <- getIsPlayable
                iid
                (InvestigatorSource iid)
                UnpaidCost
                [Window Timing.When DoNotCheckWindow]
                (toCard a)
              pure $ toModifiers
                a
                [ ActionAbilityOverride Action.Fight
                  $ CriteriaOverride
                  $ AnyCriterion [OnSameLocation, OnLocation Anywhere]
                  <> EnemyCriteria
                       (ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
                | isPlayable
                ]
            else pure []
        _ -> pure []
  getModifiersFor _ _ = pure []


instance RunMessage Marksmanship2 where
  runMessage msg e@(Marksmanship2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ CreateEffect "04104" Nothing (toSource attrs) (InvestigatorTarget iid)
        , Discard (toTarget attrs)
        ]
    _ -> Marksmanship2 <$> runMessage msg attrs


newtype Marksmanship2Effect = Marksmanship2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship2Effect :: EffectArgs -> Marksmanship2Effect
marksmanship2Effect = Marksmanship2Effect . uncurry4 (baseAttrs "04079")

instance HasModifiersFor Marksmanship2Effect where
  getModifiersFor (AbilityTarget iid ability) (Marksmanship2Effect a)
    | effectTarget a == InvestigatorTarget iid = case abilitySource ability of
      EnemySource _ -> case abilityAction ability of
        Just Action.Fight -> do
          pure $ toModifiers
            a
            [ ActionAbilityOverride Action.Fight
              $ CriteriaOverride
              $ AnyCriterion [OnSameLocation, OnLocation Anywhere]
              <> EnemyCriteria
                   (ThisEnemy $ EnemyWithoutModifier CannotBeAttacked)
            ]
        _ -> pure []
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Marksmanship2Effect where
  runMessage msg e@(Marksmanship2Effect attrs@EffectAttrs {..}) = case msg of
    FinishAction -> do
      push (DisableEffect effectId)
      pure e
    _ -> Marksmanship2Effect <$> runMessage msg attrs
