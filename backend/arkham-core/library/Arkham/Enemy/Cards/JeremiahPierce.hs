module Arkham.Enemy.Cards.JeremiahPierce (
  JeremiahPierce (..),
  jeremiahPierce,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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
        , CreateEffect (toCardCode attrs) Nothing (toSource attrs) (InvestigatorTarget iid)
        ]
      pure e
    _ -> JeremiahPierce <$> runMessage msg attrs
