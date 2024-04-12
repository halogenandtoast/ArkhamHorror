module Arkham.Enemy.Cards.JeremiahPierce (JeremiahPierce (..), jeremiahPierce, jeremiahPierceEffect) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EnemyCard JeremiahPierce
jeremiahPierce =
  enemyWith JeremiahPierce Cards.jeremiahPierce (4, Static 3, 4) (1, 1)
    $ spawnAtL
    ?~ SpawnAtFirst ["Your House", "Rivertown"]

instance HasAbilities JeremiahPierce where
  getAbilities (JeremiahPierce attrs) =
    withBaseAbilities attrs [restrictedAbility attrs 1 OnSameLocation parleyAction_]

instance RunMessage JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ AddToVictory (toTarget attrs)
        , createCardEffect Cards.jeremiahPierce Nothing (attrs.ability 1) iid
        ]
      pure e
    _ -> JeremiahPierce <$> runMessage msg attrs

newtype JeremiahPierceEffect = JeremiahPierceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierceEffect :: EffectArgs -> JeremiahPierceEffect
jeremiahPierceEffect = cardEffect JeremiahPierceEffect Cards.jeremiahPierce

instance RunMessage JeremiahPierceEffect where
  runMessage msg e@(JeremiahPierceEffect attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId attrs -> do
      pushAll [parley iid attrs iid #willpower (Fixed 4), disable attrs]
      pure e
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      push $ PlaceDoomOnAgenda n CanAdvance
      pure e
    _ -> JeremiahPierceEffect <$> runMessage msg attrs
