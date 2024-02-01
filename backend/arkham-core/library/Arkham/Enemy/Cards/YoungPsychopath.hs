module Arkham.Enemy.Cards.YoungPsychopath (
  youngPsychopath,
  YoungPsychopath (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Timing qualified as Timing

newtype YoungPsychopath = YoungPsychopath EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

youngPsychopath :: EnemyCard YoungPsychopath
youngPsychopath =
  enemy YoungPsychopath Cards.youngPsychopath (2, Static 2, 3) (1, 1)

instance HasAbilities YoungPsychopath where
  getAbilities (YoungPsychopath a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemyEngaged Timing.After You
          $ EnemyWithId
          $ toId a
      ]

instance RunMessage YoungPsychopath where
  runMessage msg e@(YoungPsychopath attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Take 1 Horror" [InvestigatorAssignDamage iid source DamageAny 0 1]
          , Label
              "Young Psycopath gets +3 fight until the end of the investigation phase"
              [ CreateWindowModifierEffect
                  EffectPhaseWindow
                  (EffectModifiers $ toModifiers attrs [Modifier.EnemyFight 3])
                  source
                  (toTarget attrs)
              ]
          ]
      pure e
    _ -> YoungPsychopath <$> runMessage msg attrs
