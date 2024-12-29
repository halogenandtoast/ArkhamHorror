module Arkham.Location.Cards.WindsweptPath (windsweptPath) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype WindsweptPath = WindsweptPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windsweptPath :: LocationCard WindsweptPath
windsweptPath = locationWith WindsweptPath Cards.windsweptPath 3 (Static 0) (connectsToL .~ adjacentLocations)

instance HasAbilities WindsweptPath where
  getAbilities (WindsweptPath a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone $ a `inRow` 0
      , mkAbility a 2 $ forced $ Moves #after You AnySource (below a) (be a)
      ]

instance HasModifiersFor WindsweptPath where
  getModifiersFor (WindsweptPath l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage WindsweptPath where
  runMessage msg l@(WindsweptPath attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs =<< perPlayer 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      placeClues (attrs.ability 1) attrs 1
      pure l
    _ -> WindsweptPath <$> liftRunMessage msg attrs
