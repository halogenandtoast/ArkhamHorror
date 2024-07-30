module Arkham.Event.Cards.ReadTheSigns2 (readTheSigns2, ReadTheSigns2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Strategy

newtype ReadTheSigns2 = ReadTheSigns2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

readTheSigns2 :: EventCard ReadTheSigns2
readTheSigns2 = event ReadTheSigns2 Cards.readTheSigns2

instance RunMessage ReadTheSigns2 where
  runMessage msg e@(ReadTheSigns2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      skillTestModifiers
        sid
        attrs
        iid
        [AddSkillValue #willpower, DiscoveredClues 1, MayIgnoreLocationEffectsAndKeywords]
      onRevealChaosTokenEffect sid IsSymbol attrs attrs do
        eventModifier attrs attrs (SetAfterPlay ReturnThisToHand)
      pushM $ mkInvestigate sid iid attrs
      pure e
    _ -> ReadTheSigns2 <$> liftRunMessage msg attrs
