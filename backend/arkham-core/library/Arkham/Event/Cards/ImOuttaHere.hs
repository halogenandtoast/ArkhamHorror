module Arkham.Event.Cards.ImOuttaHere
  ( imOuttaHere
  , ImOuttaHere(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype ImOuttaHere = ImOuttaHere EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imOuttaHere :: EventCard ImOuttaHere
imOuttaHere = event ImOuttaHere Cards.imOuttaHere

instance RunMessage ImOuttaHere where
  runMessage msg e@(ImOuttaHere attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs ->
      e <$ push (Resign iid)
    _ -> ImOuttaHere <$> runMessage msg attrs
