module Arkham.Types.Event.Cards.Oops
  ( oops
  , Oops(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.SkillTest
import Arkham.Types.Target

newtype Oops = Oops EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops :: EventCard Oops
oops = event Oops Cards.oops

instance HasActions env Oops where
  getActions iid window (Oops attrs) = getActions iid window attrs

instance HasModifiersFor env Oops where
  getModifiersFor = noModifiersFor

instance HasSkillTest env => RunMessage env Oops where
  runMessage msg e@(Oops attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just (EnemyTarget targetId) -> e <$ pushAll
          [ CancelFailedByModifierEffects
          , InvestigatorDamageEnemy iid targetId
          , Discard (toTarget attrs)
          ]
        _ -> error "Oops failed to find target"
    _ -> Oops <$> runMessage msg attrs
