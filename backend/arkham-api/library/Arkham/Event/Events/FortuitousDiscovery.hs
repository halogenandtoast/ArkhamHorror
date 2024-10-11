module Arkham.Event.Events.FortuitousDiscovery (fortuitousDiscovery, FortuitousDiscovery (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype FortuitousDiscovery = FortuitousDiscovery EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortuitousDiscovery :: EventCard FortuitousDiscovery
fortuitousDiscovery = event FortuitousDiscovery Cards.fortuitousDiscovery

instance RunMessage FortuitousDiscovery where
  runMessage msg e@(FortuitousDiscovery attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      x <- selectCount $ inDiscardOf iid <> basic (cardIs Cards.fortuitousDiscovery)
      skillTestModifiers sid (toSource attrs) iid [SkillModifier #intellect x, DiscoveredClues x]
      investigate sid iid attrs
      pure e
    _ -> FortuitousDiscovery <$> liftRunMessage msg attrs
