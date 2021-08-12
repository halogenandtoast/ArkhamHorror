module Arkham.Types.Event.Cards.Oops
  ( oops
  , Oops(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

newtype Oops = Oops EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops :: EventCard Oops
oops = event Oops Cards.oops

instance  HasActions Oops
instance HasModifiersFor env Oops

instance RunMessage env Oops where
  runMessage msg e@(Oops attrs) = case msg of
    InvestigatorPlayEvent iid eid (Just (EnemyTarget targetId)) _
      | eid == toId attrs -> e <$ pushAll
        [ CancelFailedByModifierEffects
        , InvestigatorDamageEnemy iid targetId
        , Discard (toTarget attrs)
        ]
    _ -> Oops <$> runMessage msg attrs
