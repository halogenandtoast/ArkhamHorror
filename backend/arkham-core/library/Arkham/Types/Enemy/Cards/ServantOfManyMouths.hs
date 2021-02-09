module Arkham.Types.Enemy.Cards.ServantOfManyMouths
  ( ServantOfManyMouths(..)
  , servantOfManyMouths
  ) where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype ServantOfManyMouths = ServantOfManyMouths EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfManyMouths :: EnemyId -> ServantOfManyMouths
servantOfManyMouths uuid =
  ServantOfManyMouths
    $ baseAttrs uuid "02224"
    $ (healthDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 2)
    . (evadeL .~ 1)

instance HasModifiersFor env ServantOfManyMouths where
  getModifiersFor = noModifiersFor

ability :: EnemyAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance ActionRunner env => HasActions env ServantOfManyMouths where
  getActions iid (AfterEnemyDefeated You eid) (ServantOfManyMouths attrs)
    | eid == toId attrs = pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions i window (ServantOfManyMouths attrs) = getActions i window attrs

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
        (unshiftMessage
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
