module Arkham.Event.Cards.TemptFate (
  temptFate,
  TemptFate (..),
)
where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.ChaosBag
import Arkham.Prelude

newtype TemptFate = TemptFate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temptFate :: EventCard TemptFate
temptFate = event TemptFate Cards.temptFate

instance RunMessage TemptFate where
  runMessage msg e@(TemptFate attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      b <- min 3 <$> getRemainingBlessTokens
      c <- min 3 <$> getRemainingCurseTokens
      mDrawing <- drawCardsIfCan iid attrs 1
      pushAll
        $ replicate b (AddChaosToken #bless)
        <> replicate c (AddChaosToken #curse)
        <> toList mDrawing
      pure e
    _ -> TemptFate <$> runMessage msg attrs
