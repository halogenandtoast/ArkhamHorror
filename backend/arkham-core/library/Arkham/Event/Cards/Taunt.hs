module Arkham.Event.Cards.Taunt
  ( taunt
  , Taunt(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message

newtype Taunt = Taunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taunt :: EventCard Taunt
taunt = event Taunt Cards.taunt

instance EventRunner env => RunMessage env Taunt where
  runMessage msg e@(Taunt attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList lid
      e <$ push
        (chooseSome
          iid
          "Done engaging enemies"
          [ EngageEnemy iid enemyId False | enemyId <- enemyIds ]
        )
    _ -> Taunt <$> runMessage msg attrs
