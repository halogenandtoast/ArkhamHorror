module Arkham.Event.Cards.Taunt (
  taunt,
  Taunt (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator

newtype Taunt = Taunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt :: EventCard Taunt
taunt = event Taunt Cards.taunt

instance RunMessage Taunt where
  runMessage msg e@(Taunt attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      enemyIds <- selectList $ enemiesColocatedWith iid
      player <- getPlayer iid
      e
        <$ push
          ( chooseSome
              player
              "Done engaging enemies"
              [ targetLabel enemyId [EngageEnemy iid enemyId Nothing False]
              | enemyId <- enemyIds
              ]
          )
    _ -> Taunt <$> runMessage msg attrs
