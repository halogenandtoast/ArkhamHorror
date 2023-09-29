module Arkham.Enemy.Cards.Poltergeist (
  poltergeist,
  Poltergeist (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message qualified as Msg
import Arkham.SkillType
import Arkham.Trait

newtype Poltergeist = Poltergeist EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poltergeist :: EnemyCard Poltergeist
poltergeist = enemy Poltergeist Cards.poltergeist (3, Static 2, 4) (0, 2)

instance HasAbilities Poltergeist where
  getAbilities (Poltergeist a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 OnSameLocation
          $ ActionAbility (Just Action.Parley)
          $ ActionCost 1
      ]

instance HasModifiersFor Poltergeist where
  getModifiersFor (EnemyTarget eid) (Poltergeist a)
    | toId a == eid =
        pure
          $ toModifiers
            a
            [ CannotBeDamagedByPlayerSourcesExcept
                $ SourceMatchesAny
                $ map
                  SourceWithTrait
                  [Spell, Relic]
            ]
  getModifiersFor _ _ = pure []

instance RunMessage Poltergeist where
  runMessage msg e@(Poltergeist attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          e
            <$ push
              ( parley
                  iid
                  source
                  (toTarget attrs)
                  SkillIntellect
                  3
              )
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source ->
          e
            <$ push (Msg.EnemyDamage (toId attrs) $ nonAttack source 1)
    _ -> Poltergeist <$> runMessage msg attrs
