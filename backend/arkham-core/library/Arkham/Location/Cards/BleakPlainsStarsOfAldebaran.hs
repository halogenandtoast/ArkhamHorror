module Arkham.Location.Cards.BleakPlainsStarsOfAldebaran (
  bleakPlainsStarsOfAldebaran,
  BleakPlainsStarsOfAldebaran (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype BleakPlainsStarsOfAldebaran = BleakPlainsStarsOfAldebaran LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bleakPlainsStarsOfAldebaran :: LocationCard BleakPlainsStarsOfAldebaran
bleakPlainsStarsOfAldebaran =
  locationWith
    BleakPlainsStarsOfAldebaran
    Cards.bleakPlainsStarsOfAldebaran
    4
    (PerPlayer 1)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasModifiersFor BleakPlainsStarsOfAldebaran where
  getModifiersFor (InvestigatorTarget iid) (BleakPlainsStarsOfAldebaran a) =
    pure $ toModifiers a [CannotPlay IsAlly | iid `on` a]
  getModifiersFor _ _ = pure []

instance RunMessage BleakPlainsStarsOfAldebaran where
  runMessage msg (BleakPlainsStarsOfAldebaran attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.starsOfAldebaran
      pure . BleakPlainsStarsOfAldebaran $ attrs & canBeFlippedL .~ False
    _ -> BleakPlainsStarsOfAldebaran <$> runMessage msg attrs
