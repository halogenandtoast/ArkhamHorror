module Arkham.Event.Cards.NarrowEscape
  ( narrowEscape
  , NarrowEscape(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Helpers
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Target

newtype NarrowEscape = NarrowEscape EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowEscape :: EventCard NarrowEscape
narrowEscape = event NarrowEscape Cards.narrowEscape

instance RunMessage NarrowEscape where
  runMessage msg e@(NarrowEscape attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ CancelNext (toSource attrs) AttackMessage
        , CreateWindowModifierEffect
          (FirstEffectWindow [EffectSkillTestWindow, EffectTurnWindow])
          (EffectModifiers $ toModifiers attrs [AnySkillValue 2])
          (toSource attrs)
          (InvestigatorTarget iid)
        , Discard (EventTarget eid)
        ]
      pure e
    _ -> NarrowEscape <$> runMessage msg attrs
