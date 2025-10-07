module Arkham.Location.Cards.RainyLondonStreets (rainyLondonStreets) where

import Arkham.Ability
import Arkham.Helpers.Act
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.RiddlesAndRain.Helpers

newtype RainyLondonStreets = RainyLondonStreets LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rainyLondonStreets :: LocationCard RainyLondonStreets
rainyLondonStreets = symbolLabel $ location RainyLondonStreets Cards.rainyLondonStreets 1 (PerPlayer 2)

instance HasModifiersFor RainyLondonStreets where
  getModifiersFor (RainyLondonStreets a) = do
    n <- getCurrentActStep
    modifySelf a [ShroudModifier n]

instance HasAbilities RainyLondonStreets where
  getAbilities (RainyLondonStreets a) =
    extendRevealed
      a
      [ restricted a 1 (thisExists a LocationWithoutClues) $ forced AnyWindow
      , scenarioI18n $ withI18nTooltip "rainyLondonStreets.resign" $ locationResignAction a
      ]

instance RunMessage RainyLondonStreets where
  runMessage msg l@(RainyLondonStreets attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      placeClues (attrs.ability 1) attrs n
      pure l
    _ -> RainyLondonStreets <$> liftRunMessage msg attrs
