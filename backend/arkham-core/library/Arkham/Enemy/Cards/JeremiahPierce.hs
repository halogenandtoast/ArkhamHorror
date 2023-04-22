module Arkham.Enemy.Cards.JeremiahPierce
  ( JeremiahPierce(..)
  , jeremiahPierce
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action hiding ( Ability )
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Source

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EnemyCard JeremiahPierce
jeremiahPierce = enemyWith
  JeremiahPierce
  Cards.jeremiahPierce
  (4, Static 3, 4)
  (1, 1)
  (spawnAtL ?~ SpawnLocation
    (FirstLocation
      [LocationWithTitle "Your House", LocationWithTitle "Rivertown"]
    )
  )

instance HasAbilities JeremiahPierce where
  getAbilities (JeremiahPierce attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 OnSameLocation
      $ ActionAbility (Just Parley)
      $ ActionCost 1
    ]

instance RunMessage JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid (EnemySource eid) 1 _ _ | eid == enemyId -> e <$ pushAll
      [ AddToVictory (EnemyTarget enemyId)
      , CreateEffect
        (toCardCode attrs)
        Nothing
        (toSource attrs)
        (InvestigatorTarget iid)
      ]
    _ -> JeremiahPierce <$> runMessage msg attrs
