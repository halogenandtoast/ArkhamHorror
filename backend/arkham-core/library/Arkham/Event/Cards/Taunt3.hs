module Arkham.Event.Cards.Taunt3
  ( taunt3
  , Taunt3(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype Taunt3 = Taunt3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt3 :: EventCard Taunt3
taunt3 = event Taunt3 Cards.taunt3

instance EventRunner env => RunMessage env Taunt3 where
  runMessage msg e@(Taunt3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList lid
      e <$ push
        (chooseSome
          iid
          "Done engaging enemies"
          [ TargetLabel
              (EnemyTarget enemyId)
              [ EngageEnemy iid enemyId False
              , InvestigatorDamageEnemy iid enemyId (toSource attrs)
              , DrawCards iid 1 False
              ]
          | enemyId <- enemyIds
          ]
        )
    _ -> Taunt3 <$> runMessage msg attrs
