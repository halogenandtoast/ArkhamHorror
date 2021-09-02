module Arkham.Types.Enemy.Cards.JeremiahPierce
  ( JeremiahPierce(..)
  , jeremiahPierce
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EnemyCard JeremiahPierce
jeremiahPierce = enemyWith
  JeremiahPierce
  Cards.jeremiahPierce
  (4, Static 3, 4)
  (1, 1)
  (spawnAtL ?~ FirstLocation
    [LocationWithTitle "Your House", LocationWithTitle "Rivertown"]
  )

instance HasAbilities JeremiahPierce where
  getAbilities (JeremiahPierce attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 OnSameLocation
      $ ActionAbility (Just Parley)
      $ ActionCost 1
    ]

instance EnemyRunner env => RunMessage env JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid (EnemySource eid) _ 1 _ | eid == enemyId -> e <$ pushAll
      [ AddToVictory (EnemyTarget enemyId)
      , CreateEffect
        (toCardCode attrs)
        Nothing
        (toSource attrs)
        (InvestigatorTarget iid)
      ]
    _ -> JeremiahPierce <$> runMessage msg attrs
