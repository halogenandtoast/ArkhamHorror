module Arkham.Enemy.Cards.TheDreamEaters.TheSearchForKadath.HordeOfNight (hordeOfNight) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype HordeOfNight = HordeOfNight EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hordeOfNight :: EnemyCard HordeOfNight
hordeOfNight = enemy HordeOfNight Cards.hordeOfNight

instance HasModifiersFor HordeOfNight where
  getModifiersFor (HordeOfNight a) = do
    isHost <- toId a <=~> IsHost
    modifySelf a [if isHost then ExhaustIfDefeated else CannotAttack]
