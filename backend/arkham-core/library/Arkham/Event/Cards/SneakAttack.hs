module Arkham.Event.Cards.SneakAttack where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message

newtype SneakAttack = SneakAttack EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sneakAttack :: EventCard SneakAttack
sneakAttack = event SneakAttack Cards.sneakAttack

instance EventRunner env => RunMessage SneakAttack where
  runMessage msg e@(SneakAttack attrs) = case msg of
    InvestigatorPlayEvent you eid _ _ _ | eid == toId attrs -> do
      yourLocation <- LocationWithId <$> getId you
      enemies <- selectList $ ExhaustedEnemy <> EnemyAt yourLocation
      e <$ pushAll
        ([ EnemyDamage enemy you (toSource attrs) NonAttackDamageEffect 2
         | enemy <- enemies
         ]
        <> [Discard $ toTarget attrs]
        )
    _ -> SneakAttack <$> runMessage msg attrs
