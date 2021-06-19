module Arkham.Types.Treachery.Cards.ToweringBeasts
  ( toweringBeasts
  , ToweringBeasts(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

newtype ToweringBeasts = ToweringBeasts TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringBeasts :: TreacheryId -> a -> ToweringBeasts
toweringBeasts uuid _ = ToweringBeasts $ baseAttrs uuid "02256"

instance HasModifiersFor env ToweringBeasts where
  getModifiersFor _ (EnemyTarget eid) (ToweringBeasts attrs)
    | treacheryOnEnemy eid attrs = pure
    $ toModifiers attrs [EnemyFight 1, HealthModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasActions env ToweringBeasts where
  getActions i window (ToweringBeasts attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ToweringBeasts where
  runMessage msg t@(ToweringBeasts attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      broodOfYogSothoth <- getSetList @EnemyId (CardCode "02255")
      case broodOfYogSothoth of
        [] -> t <$ unshiftMessage (Discard $ toTarget attrs)
        xs -> do
          locationId <- getId @LocationId iid
          broodWithLocationIds <- for xs $ \x -> (x, ) <$> getId @LocationId x
          t <$ unshiftMessages
            [ chooseOne
              iid
              [ TargetLabel
                  (EnemyTarget eid)
                  ([AttachTreachery (toId attrs) (EnemyTarget eid)]
                  <> [ InvestigatorAssignDamage iid source DamageAny 1 0
                     | lid == locationId
                     ]
                  )
              | (eid, lid) <- broodWithLocationIds
              ]
            , Discard (toTarget attrs)
            ]
    _ -> ToweringBeasts <$> runMessage msg attrs
