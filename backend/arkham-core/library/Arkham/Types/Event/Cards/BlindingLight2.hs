module Arkham.Types.Event.Cards.BlindingLight2
  ( blindingLight2
  , BlindingLight2(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype BlindingLight2 = BlindingLight2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: EventCard BlindingLight2
blindingLight2 = event BlindingLight2 Cards.blindingLight2

instance EventRunner env => RunMessage env BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ CreateEffect "01069" Nothing (toSource attrs) (InvestigatorTarget iid)
      , CreateEffect "01069" Nothing (toSource attrs) SkillTestTarget
      , ChooseEvadeEnemy
        iid
        (EventSource eid)
        Nothing
        SkillWillpower
        AnyEnemy
        False
      , Discard (EventTarget eid)
      ]
    _ -> BlindingLight2 <$> runMessage msg attrs
