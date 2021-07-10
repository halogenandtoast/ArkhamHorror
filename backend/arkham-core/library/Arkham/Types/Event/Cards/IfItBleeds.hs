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
import Arkham.Types.Window

newtype IfItBleeds = IfItBleeds EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifItBleeds :: EventCard IfItBleeds
ifItBleeds = event IfItBleeds Cards.ifItBleeds

instance HasActions env IfItBleeds where
  getActions iid window (IfItBleeds attrs) = getActions iid window attrs

instance HasModifiersFor env IfItBleeds

getWindowEnemyIds :: [Window] -> [EnemyId]
getWindowEnemyIds = mapMaybe \case
  AfterEnemyDefeated You eid -> Just eid
  _ -> Nothing

instance
  ( HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  , HasCount SanityDamageCount env EnemyId
  )
  => RunMessage env IfItBleeds where
  runMessage msg e@(IfItBleeds attrs) = case msg of
    InvestigatorPlayFastEvent iid eid _ windows | eid == toId attrs -> do
      let enemyIds = getWindowEnemyIds windows
      enemyIdsWithHorrorValue <- traverse
        (traverseToSnd (fmap unSanityDamageCount . getCount))
        enemyIds
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      e <$ push
        (chooseOne
          iid
          [ TargetLabel
              (EnemyTarget enemyId)
              [ HealHorror (InvestigatorTarget iid') horrorValue
              | iid' <- investigatorIds
              ]
          | (enemyId, horrorValue) <- enemyIdsWithHorrorValue
          ]
        )
    _ -> IfItBleeds <$> runMessage msg attrs
