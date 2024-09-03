module Arkham.Enemy.Cards.RiftSeeker (
  riftSeeker,
  RiftSeeker (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype RiftSeeker = RiftSeeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riftSeeker :: EnemyCard RiftSeeker
riftSeeker = enemy RiftSeeker Cards.riftSeeker (3, Static 3, 4) (1, 1)

instance HasAbilities RiftSeeker where
  getAbilities (RiftSeeker a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ EnemyAttacks #after You AnyEnemyAttack
          $ EnemyWithId (toId a)
      , restrictedAbility a 2 OnSameLocation
          $ parleyAction
          $ HorrorCost (toSource a) YouTarget 2
          <> DoomCost (toSource a) (AgendaMatcherTarget AnyAgenda) 1
      ]

instance RunMessage RiftSeeker where
  runMessage msg e@(RiftSeeker attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      agendas <- selectMap AgendaTarget AnyAgenda
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
              "take 1 additional damage and 1 additional horror"
              [ CreateWindowModifierEffect
                  EffectAttackWindow
                  ( EffectModifiers $ toModifiers attrs [DamageDealt 1, HorrorDealt 1]
                  )
                  (toSource attrs)
                  (toTarget attrs)
              ]
          , Label
              "Place 1 doom on each agenda"
              [PlaceDoom (toAbilitySource attrs 1) target 1 | target <- agendas]
          ]
      pure e
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure e
    _ -> RiftSeeker <$> runMessage msg attrs
