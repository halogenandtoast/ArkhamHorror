module Arkham.Event.Cards.Pilfer (
  pilfer,
  Pilfer (..),
)
where

import Arkham.Prelude

import Arkham.Aspect
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigate

newtype Pilfer = Pilfer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

pilfer :: EventCard Pilfer
pilfer = event Pilfer Cards.pilfer

instance RunMessage Pilfer where
  runMessage msg e@(Pilfer attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigation <- aspect iid attrs (#agility `InsteadOf` #intellect) (mkInvestigate iid attrs)
      pushAll $ skillTestModifier attrs iid (DiscoveredClues 2) : leftOr investigation
      pure e
    _ -> Pilfer <$> runMessage msg attrs
