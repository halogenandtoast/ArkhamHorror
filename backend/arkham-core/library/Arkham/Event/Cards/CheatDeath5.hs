module Arkham.Event.Cards.CheatDeath5 (
  cheatDeath5,
  CheatDeath5 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Movement

newtype CheatDeath5 = CheatDeath5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

cheatDeath5 :: EventCard CheatDeath5
cheatDeath5 = eventWith CheatDeath5 Cards.cheatDeath5 $ afterPlayL .~ RemoveThisFromGame

instance RunMessage CheatDeath5 where
  runMessage msg e@(CheatDeath5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId iid
      treacheries <- selectList $ TreacheryInThreatAreaOf $ InvestigatorWithId iid
      locations <-
        selectList
          $ RevealedLocation
          <> LocationWithoutEnemies
          <> NotLocation (locationWithInvestigator iid)
          <> canEnterLocation iid
      yourTurn <- member iid <$> select TurnInvestigator

      replaceMessageMatching
        \case
          InvestigatorWhenDefeated _ iid' -> iid == iid'
          _ -> False
        \case
          InvestigatorWhenDefeated source' _ -> [checkDefeated source' iid]
          _ -> error "invalid match"

      mHealHorror <- getHealHorrorMessage attrs 2 iid
      healable <- canHaveDamageHealed attrs iid
      player <- getPlayer iid

      pushAll
        $ map (DisengageEnemy iid) enemies
        <> map (toDiscardBy iid attrs) treacheries
        <> maybeToList mHealHorror
        <> [HealDamage (InvestigatorTarget iid) (toSource attrs) 2 | healable]
        <> [ chooseOrRunOne player $ targetLabels locations (only . MoveTo . move (toSource attrs) iid)
           | notNull locations
           ]
        <> [ChooseEndTurn iid | yourTurn]
      pure e
    _ -> CheatDeath5 <$> runMessage msg attrs
