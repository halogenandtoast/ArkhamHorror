module Arkham.Event.Events.NarrowEscape (narrowEscape) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Window

newtype NarrowEscape = NarrowEscape EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowEscape :: EventCard NarrowEscape
narrowEscape = event NarrowEscape Cards.narrowEscape

instance RunMessage NarrowEscape where
  runMessage msg e@(NarrowEscape attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      cancelAttack attrs (getAttackDetails attrs.windows)
      nextSkillTestModifier iid attrs iid (AnySkillValue 2)
      pure e
    _ -> NarrowEscape <$> liftRunMessage msg attrs
