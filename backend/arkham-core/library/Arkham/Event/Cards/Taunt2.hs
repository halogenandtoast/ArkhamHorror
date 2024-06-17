module Arkham.Event.Cards.Taunt2 (taunt2, Taunt2 (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Prelude

newtype Taunt2 = Taunt2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt2 :: EventCard Taunt2
taunt2 = event Taunt2 Cards.taunt2

instance RunMessage Taunt2 where
  runMessage msg e@(Taunt2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemies <- map (\enemyId -> (enemyId, drawCards iid attrs 1)) <$> select (enemiesColocatedWith iid)
      player <- getPlayer iid
      push
        $ chooseSome
          player
          "Done engaging enemies"
          [ targetLabel enemyId [EngageEnemy iid enemyId Nothing False, drawing]
          | (enemyId, drawing) <- enemies
          ]
      pure e
    _ -> Taunt2 <$> runMessage msg attrs
