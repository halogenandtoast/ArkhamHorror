module Arkham.Types.Enemy.Cards.YoungPsychopath
  ( youngPsychopath
  , YoungPsychopath(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Timing qualified as Timing

newtype YoungPsychopath = YoungPsychopath EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngPsychopath :: EnemyCard YoungPsychopath
youngPsychopath =
  enemy YoungPsychopath Cards.youngPsychopath (2, Static 2, 3) (1, 1)

instance HasAbilities YoungPsychopath where
  getAbilities (YoungPsychopath a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyEngaged Timing.After You
      $ EnemyWithId
      $ toId a
    ]

instance EnemyRunner env => RunMessage env YoungPsychopath where
  runMessage msg e@(YoungPsychopath attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> e <$ push
      (chooseOne
        iid
        [ Label
          "Take 1 Horror"
          [InvestigatorAssignDamage iid source DamageAny 0 1]
        , Label
          "Young Psycopath gets +3 fight until the end of the investigation phase"
          [ CreateWindowModifierEffect
              EffectPhaseWindow
              (EffectModifiers $ toModifiers attrs [EnemyFight 3])
              source
              (toTarget attrs)
          ]
        ]
      )
    _ -> YoungPsychopath <$> runMessage msg attrs
