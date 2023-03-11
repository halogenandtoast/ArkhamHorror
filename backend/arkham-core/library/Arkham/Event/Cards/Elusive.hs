module Arkham.Event.Cards.Elusive where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement

newtype Elusive = Elusive EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elusive :: EventCard Elusive
elusive = event Elusive Cards.elusive

instance RunMessage Elusive where
  runMessage msg e@(Elusive attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      enemyIds <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId iid
      targets <- selectList $ EmptyLocation <> RevealedLocation
      pushAll
        $ [ DisengageEnemy iid enemyId | enemyId <- enemyIds ]
        <> [ chooseOrRunOne
               iid
               [ targetLabel lid [MoveTo $ move (toSource attrs) iid lid]
               | lid <- targets
               ]
           | notNull targets
           ]
        <> map EnemyCheckEngagement enemyIds
      pure e
    _ -> Elusive <$> runMessage msg attrs
