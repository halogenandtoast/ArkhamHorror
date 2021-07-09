module Arkham.Types.Event.Cards.Taunt3
  ( taunt3
  , Taunt3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype Taunt3 = Taunt3 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt3 :: EventCard Taunt3
taunt3 = event Taunt3 Cards.taunt3

instance HasActions env Taunt3 where
  getActions iid window (Taunt3 attrs) = getActions iid window attrs

instance HasModifiersFor env Taunt3 where
  getModifiersFor = noModifiersFor

instance (EventRunner env) => RunMessage env Taunt3 where
  runMessage msg e@(Taunt3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList lid
      e <$ push
        (chooseSome
          iid
          [ TargetLabel
              (EnemyTarget enemyId)
              [ EngageEnemy iid enemyId False
              , InvestigatorDamageEnemy iid enemyId
              , DrawCards iid 1 False
              ]
          | enemyId <- enemyIds
          ]
        )
    _ -> Taunt3 <$> runMessage msg attrs
