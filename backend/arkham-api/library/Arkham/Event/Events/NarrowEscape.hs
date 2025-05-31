module Arkham.Event.Events.NarrowEscape (narrowEscape) where

import Arkham.Effect.Window
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Matcher

newtype NarrowEscape = NarrowEscape EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowEscape :: EventCard NarrowEscape
narrowEscape = event NarrowEscape Cards.narrowEscape

instance RunMessage NarrowEscape where
  runMessage msg e@(NarrowEscape attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      iid' <- selectJust TurnInvestigator
      cancelAttack attrs (getAttackDetails attrs.windows)
      ems <- effectModifiers attrs [AnySkillValue 2]
      push
        $ CreateWindowModifierEffect
          (FirstEffectWindow [EffectNextSkillTestWindow iid, EffectTurnWindow iid'])
          ems
          (toSource attrs)
          (InvestigatorTarget iid)
      pure e
    _ -> NarrowEscape <$> liftRunMessage msg attrs
