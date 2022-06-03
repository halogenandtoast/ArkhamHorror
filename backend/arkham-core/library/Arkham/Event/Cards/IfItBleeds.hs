module Arkham.Event.Cards.IfItBleeds
  ( ifItBleeds
  , IfItBleeds(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Id
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Query
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype IfItBleeds = IfItBleeds EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifItBleeds :: EventCard IfItBleeds
ifItBleeds = event IfItBleeds Cards.ifItBleeds

getWindowEnemyIds :: InvestigatorId -> [Window] -> [EnemyId]
getWindowEnemyIds iid = mapMaybe \case
  Window Timing.After (EnemyDefeated who eid) | iid == who -> Just eid
  _ -> Nothing

instance
  ( HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  , HasCount SanityDamageCount env EnemyId
  )
  => RunMessage IfItBleeds where
  runMessage msg e@(IfItBleeds attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == toId attrs -> do
      let enemyIds = getWindowEnemyIds iid windows
      enemyIdsWithHorrorValue <- traverse
        (traverseToSnd (fmap unSanityDamageCount . getCount))
        enemyIds
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      e <$ pushAll
        (chooseOne
            iid
            [ TargetLabel
                (EnemyTarget enemyId)
                [ HealHorror (InvestigatorTarget iid') horrorValue
                | iid' <- investigatorIds
                ]
            | (enemyId, horrorValue) <- enemyIdsWithHorrorValue
            ]
        : [Discard $ toTarget attrs]
        )
    _ -> IfItBleeds <$> runMessage msg attrs
