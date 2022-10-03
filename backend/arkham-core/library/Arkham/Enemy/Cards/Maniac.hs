module Arkham.Enemy.Cards.Maniac
  ( maniac
  , Maniac(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype Maniac = Maniac EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities Maniac where
  getAbilities (Maniac a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyEngaged Timing.After You
      $ EnemyWithId
      $ toId a
    ]

maniac :: EnemyCard Maniac
maniac = enemy Maniac Cards.maniac (3, Static 4, 1) (1, 0)

instance RunMessage Maniac where
  runMessage msg e@(Maniac attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> e <$ pushAll
      [ InvestigatorAssignDamage iid source DamageAny 1 0
      , Msg.EnemyDamage (toId attrs) iid source NonAttackDamageEffect 1
      ]
    _ -> Maniac <$> runMessage msg attrs
