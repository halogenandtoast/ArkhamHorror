module Arkham.Event.Cards.IfItBleeds (
  ifItBleeds,
  IfItBleeds (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype IfItBleeds = IfItBleeds EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ifItBleeds :: EventCard IfItBleeds
ifItBleeds = event IfItBleeds Cards.ifItBleeds

getWindowEnemyIds :: InvestigatorId -> [Window] -> [EnemyId]
getWindowEnemyIds iid = mapMaybe \case
  Window Timing.After (EnemyDefeated (Just who) _ eid) _ | iid == who -> Just eid
  _ -> Nothing

instance RunMessage IfItBleeds where
  runMessage msg e@(IfItBleeds attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (getWindowEnemyIds iid -> enemyIds) _ | eid == toId attrs -> do
      choices <- for enemyIds $ \enemyId -> do
        horrorValue <- field EnemySanityDamage enemyId
        healMessages <-
          map snd
            <$> ( getInvestigatorsWithHealHorror
                    attrs
                    horrorValue
                    =<< guardAffectsColocated iid
                )
        pure $ targetLabel enemyId healMessages
      player <- getPlayer iid
      push $ chooseOne player choices
      pure e
    _ -> IfItBleeds <$> runMessage msg attrs
