module Arkham.Event.Cards.CunningDistraction where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message

newtype CunningDistraction = CunningDistraction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunningDistraction :: EventCard CunningDistraction
cunningDistraction = event CunningDistraction Cards.cunningDistraction

instance RunMessage CunningDistraction where
  runMessage msg e@(CunningDistraction attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      enemyIds <-
        selectList $ EnemyAt $ LocationWithInvestigator $ InvestigatorWithId iid
      pushAll $ map (EnemyEvaded iid) enemyIds <> [discard attrs]
      pure e
    _ -> CunningDistraction <$> runMessage msg attrs
