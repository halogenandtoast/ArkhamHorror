module Arkham.Enemy.Cards.RiftSeeker
  ( riftSeeker
  , RiftSeeker(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EnemyAttacks )
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype RiftSeeker = RiftSeeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riftSeeker :: EnemyCard RiftSeeker
riftSeeker = enemy RiftSeeker Cards.riftSeeker (3, Static 3, 3) (1, 1)

instance HasAbilities RiftSeeker where
  getAbilities (RiftSeeker a) = withBaseAbilities
    a
    [ mkAbility a 1
    $ ForcedAbility
    $ EnemyAttacks Timing.After You AnyEnemyAttack
    $ EnemyWithId (toId a)
    , restrictedAbility a 2 OnSameLocation
    $ ActionAbility (Just Action.Parley)
    $ HorrorCost (toSource a) YouTarget 2
    <> DoomCost (toSource a) (AgendaMatcherTarget AnyAgenda) 1
    ]

instance RunMessage RiftSeeker where
  runMessage msg e@(RiftSeeker attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      agendas <- selectListMap AgendaTarget AnyAgenda
      push $ chooseOne
        iid
        [ Label
          "take 1 additional damage and 1 additional horror"
          [ CreateWindowModifierEffect
              EffectAttackWindow
              (EffectModifiers $ toModifiers attrs [DamageDealt 1, HorrorDealt 1]
              )
              (toSource attrs)
              (toTarget attrs)
          ]
        , Label
          "Place 1 doom on each agenda"
          [ PlaceDoom target 1 | target <- agendas ]
        ]
      pure e
    UseCardAbility _iid source 2 _ _ | isSource attrs source ->
      e <$ push (Discard (toAbilitySource attrs 2) $ toTarget attrs)
    _ -> RiftSeeker <$> runMessage msg attrs
