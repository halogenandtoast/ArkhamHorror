module Arkham.Types.Event.Cards.IfItBleeds
  ( ifItBleeds
  , IfItBleeds(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window
import qualified Arkham.Types.Window as W

newtype IfItBleeds = IfItBleeds EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifItBleeds :: EventCard IfItBleeds
ifItBleeds = event IfItBleeds Cards.ifItBleeds

instance HasActions IfItBleeds
instance HasModifiersFor env IfItBleeds

getWindowEnemyIds :: InvestigatorId -> [Window] -> [EnemyId]
getWindowEnemyIds iid = mapMaybe \case
  Window Timing.After (W.EnemyDefeated who eid) | iid == who -> Just eid
  _ -> Nothing

instance
  ( HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  , HasCount SanityDamageCount env EnemyId
  )
  => RunMessage env IfItBleeds where
  runMessage msg e@(IfItBleeds attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows | eid == toId attrs -> do
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
