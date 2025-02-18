module Arkham.Event.Events.WingingIt (wingingIt) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Modifier
import Arkham.Projection
import Arkham.Zone qualified as Zone

newtype WingingIt = WingingIt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingingIt :: EventCard WingingIt
wingingIt = event WingingIt Cards.wingingIt

instance RunMessage WingingIt where
  runMessage msg e@(WingingIt attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid (is attrs -> True) _ _ zone -> do
      lid <- fieldJust InvestigatorLocation iid
      sid <- getRandom
      skillTestModifier sid attrs lid (ShroudModifier (-1))
      when (zone == Zone.FromDiscard) do
        skillTestModifier sid attrs iid (DiscoveredClues 1)
      investigate sid iid attrs
      when (zone == Zone.FromDiscard) $ shuffleIntoDeck iid attrs
      pure e
    _ -> WingingIt <$> liftRunMessage msg attrs
