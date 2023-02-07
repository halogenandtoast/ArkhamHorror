module Arkham.Event.Cards.ImDoneRunnin
  ( imDoneRunnin
  , imDoneRunninEffect
  , ImDoneRunnin(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher hiding ( EnemyEvaded )
import Arkham.Message
import Arkham.Target

newtype ImDoneRunnin = ImDoneRunnin EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imDoneRunnin :: EventCard ImDoneRunnin
imDoneRunnin = event ImDoneRunnin Cards.imDoneRunnin

instance RunMessage ImDoneRunnin where
  runMessage msg e@(ImDoneRunnin attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- selectList $ EnemyAt (locationWithInvestigator iid)
      pushAll
        $ map (Ready . EnemyTarget) enemies
        <> [ EngageEnemy iid enemy False | enemy <- enemies ]
        <> [Discard (toTarget attrs)]
      pure e
    _ -> ImDoneRunnin <$> runMessage msg attrs

newtype ImDoneRunninEffect = ImDoneRunninEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imDoneRunninEffect :: EffectArgs -> ImDoneRunninEffect
imDoneRunninEffect = cardEffect ImDoneRunninEffect Cards.imDoneRunnin

instance HasModifiersFor ImDoneRunninEffect where
  getModifiersFor target (ImDoneRunninEffect a) | effectTarget a == target = do
    pure $ toModifiers a [DoNotExhaustEvaded, DoNotDisengageEvaded]
  getModifiersFor _ _ = pure []

instance RunMessage ImDoneRunninEffect where
  runMessage msg e@(ImDoneRunninEffect attrs@EffectAttrs {..}) = case msg of
    EnemyEvaded iid eid | InvestigatorTarget iid == effectTarget -> do
      canDamage <- eid <=~> EnemyCanBeDamagedBySource effectSource
      when canDamage $ push $ chooseOne
        iid
        [ Label "Do not damage enemy" []
        , Label "Damage enemy" [EnemyDamage eid $ nonAttack effectSource 1]
        ]
      pure e
    EndTurn _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> ImDoneRunninEffect <$> runMessage msg attrs
