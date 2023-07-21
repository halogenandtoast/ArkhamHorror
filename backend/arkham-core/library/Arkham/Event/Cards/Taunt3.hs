module Arkham.Event.Cards.Taunt3 (
  taunt3,
  Taunt3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Message

newtype Taunt3 = Taunt3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt3 :: EventCard Taunt3
taunt3 = event Taunt3 Cards.taunt3

instance RunMessage Taunt3 where
  runMessage msg e@(Taunt3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      enemyIds <- selectList $ enemiesColocatedWith iid
      enemies <- forToSnd enemyIds $ \_ -> drawCards iid attrs 1
      push $
        chooseSome
          iid
          "Done engaging enemies"
          [ targetLabel
            enemyId
            [ EngageEnemy iid enemyId False
            , InvestigatorDamageEnemy iid enemyId (toSource attrs)
            , drawing
            ]
          | (enemyId, drawing) <- enemies
          ]
      pure e
    _ -> Taunt3 <$> runMessage msg attrs
