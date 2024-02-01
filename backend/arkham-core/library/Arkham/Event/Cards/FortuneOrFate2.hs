module Arkham.Event.Cards.FortuneOrFate2 (
  fortuneOrFate2,
  FortuneOrFate2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype FortuneOrFate2 = FortuneOrFate2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

fortuneOrFate2 :: EventCard FortuneOrFate2
fortuneOrFate2 = event FortuneOrFate2 Cards.fortuneOrFate2

getDoomTarget :: [Window] -> Target
getDoomTarget [] = error "wrong window"
getDoomTarget ((windowType -> Window.PlacedDoom _ doomTarget _) : _) = doomTarget
getDoomTarget (_ : xs) = getDoomTarget xs

instance RunMessage FortuneOrFate2 where
  runMessage msg e@(FortuneOrFate2 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ (getDoomTarget -> doomTarget) _ | eid == toId attrs -> do
      push $ RemoveDoom (toSource attrs) doomTarget 1
      pure e
    _ -> FortuneOrFate2 <$> runMessage msg attrs
