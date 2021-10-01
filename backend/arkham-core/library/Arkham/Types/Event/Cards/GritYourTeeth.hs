module Arkham.Types.Event.Cards.GritYourTeeth
  ( gritYourTeeth
  , GritYourTeeth(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype GritYourTeeth = GritYourTeeth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gritYourTeeth :: EventCard GritYourTeeth
gritYourTeeth = event GritYourTeeth Cards.gritYourTeeth

instance EventRunner env => RunMessage env GritYourTeeth where
  runMessage msg e@(GritYourTeeth attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ CreateWindowModifierEffect
          EffectRoundWindow
          (EffectModifiers $ toModifiers attrs [AnySkillValue 1])
          (toSource attrs)
          (InvestigatorTarget iid)
        , Discard (toTarget attrs)
        ]
    _ -> GritYourTeeth <$> runMessage msg attrs
