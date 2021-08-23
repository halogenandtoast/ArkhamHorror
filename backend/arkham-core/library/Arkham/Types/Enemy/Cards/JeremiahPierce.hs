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
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype JeremiahPierce = JeremiahPierce EnemyAttrs
  deriving anyclass IsEnemy
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

instance HasModifiersFor env JeremiahPierce

instance ActionRunner env => HasAbilities env JeremiahPierce where
  getAbilities iid window@(Window Timing.When NonFast) (JeremiahPierce attrs@EnemyAttrs {..})
    = withBaseAbilities iid window attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ mkAbility attrs 1 $ ActionAbility (Just Parley) (ActionCost 1)
        | locationId == enemyLocation
        ]
  getAbilities _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env JeremiahPierce where
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
