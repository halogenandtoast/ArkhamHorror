module Arkham.Event.Cards.IfItBleeds
  ( ifItBleeds
  , IfItBleeds(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Matcher hiding ( EnemyDefeated )
import Arkham.Message hiding ( EnemyDefeated )
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype IfItBleeds = IfItBleeds EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifItBleeds :: EventCard IfItBleeds
ifItBleeds = event IfItBleeds Cards.ifItBleeds

getWindowEnemyIds :: InvestigatorId -> [Window] -> [EnemyId]
getWindowEnemyIds iid = mapMaybe \case
  Window Timing.After (EnemyDefeated (Just who) eid) | iid == who -> Just eid
  _ -> Nothing

instance RunMessage IfItBleeds where
  runMessage msg e@(IfItBleeds attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == toId attrs -> do
      let enemyIds = getWindowEnemyIds iid windows
      enemyIdsWithHorrorValue <- forToSnd enemyIds (field EnemySanityDamage)
      choices <- for enemyIdsWithHorrorValue $ \(enemyId, horrorValue) -> do
        healMessages <- map snd <$> getInvestigatorsWithHealHorror
          attrs
          horrorValue
          (colocatedWith iid)
        pure $ targetLabel enemyId healMessages
      pushAll [chooseOne iid choices, discard attrs]
      pure e
    _ -> IfItBleeds <$> runMessage msg attrs
