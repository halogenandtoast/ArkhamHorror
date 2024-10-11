module Arkham.Event.Events.NarrowEscape (narrowEscape, NarrowEscape (..)) where

import Arkham.Classes
import Arkham.Effect.Window
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype NarrowEscape = NarrowEscape EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowEscape :: EventCard NarrowEscape
narrowEscape = event NarrowEscape Cards.narrowEscape

instance RunMessage NarrowEscape where
  runMessage msg e@(NarrowEscape attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iid' <- selectJust TurnInvestigator
      ems <- effectModifiers attrs [AnySkillValue 2]
      pushAll
        [ CancelNext (toSource attrs) AttackMessage
        , CreateWindowModifierEffect
            (FirstEffectWindow [EffectNextSkillTestWindow, EffectTurnWindow iid'])
            ems
            (toSource attrs)
            (InvestigatorTarget iid)
        ]
      pure e
    _ -> NarrowEscape <$> runMessage msg attrs
