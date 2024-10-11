module Arkham.Event.Events.ReadTheSigns (readTheSigns, ReadTheSigns (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype ReadTheSigns = ReadTheSigns EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

readTheSigns :: EventCard ReadTheSigns
readTheSigns = event ReadTheSigns Cards.readTheSigns

instance RunMessage ReadTheSigns where
  runMessage msg e@(ReadTheSigns attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      sid <- getRandom
      skillTestModifiers
        sid
        attrs
        iid
        [AddSkillValue #willpower, DiscoveredClues 1, MayIgnoreLocationEffectsAndKeywords]
      investigate sid iid attrs
      pure e
    _ -> ReadTheSigns <$> liftRunMessage msg attrs
