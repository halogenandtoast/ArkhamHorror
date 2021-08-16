module Arkham.Types.Enemy.Cards.ServantOfManyMouths
  ( ServantOfManyMouths(..)
  , servantOfManyMouths
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Window

newtype ServantOfManyMouths = ServantOfManyMouths EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfManyMouths :: EnemyCard ServantOfManyMouths
servantOfManyMouths =
  enemy ServantOfManyMouths Cards.servantOfManyMouths (3, Static 2, 1) (2, 0)

instance HasModifiersFor env ServantOfManyMouths

ability :: EnemyAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (LegacyReactionAbility Free)

instance ActionRunner env => HasAbilities env ServantOfManyMouths where
  getAbilities iid (AfterEnemyDefeated who eid) (ServantOfManyMouths attrs)
    | eid == toId attrs && iid == who = pure [ability attrs]
  getAbilities i window (ServantOfManyMouths attrs) = getAbilities i window attrs

instance EnemyRunner env => RunMessage env ServantOfManyMouths where
  runMessage msg e@(ServantOfManyMouths attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationIds <- getSetList ()
      locationsWithClues <- filterM
        (fmap ((> 0) . unClueCount) . getCount)
        locationIds
      e <$ unless
        (null locationsWithClues)
        (push
          (chooseOne
            iid
            [ TargetLabel
                (LocationTarget lid)
                [DiscoverCluesAtLocation iid lid 1 Nothing]
            | lid <- locationsWithClues
            ]
          )
        )
    _ -> ServantOfManyMouths <$> runMessage msg attrs
