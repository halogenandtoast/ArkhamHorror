module Arkham.Event.Cards.CunningDistraction where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (EnemyEvaded)

newtype CunningDistraction = CunningDistraction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunningDistraction :: EventCard CunningDistraction
cunningDistraction = event CunningDistraction Cards.cunningDistraction

instance RunMessage CunningDistraction where
  runMessage msg e@(CunningDistraction attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      enemyIds <- selectList $ enemyAtLocationWith iid
      pushAll $ map (EnemyEvaded iid) enemyIds
      pure e
    _ -> CunningDistraction <$> runMessage msg attrs
