{- | Drugstore (008) -- buy the clues back with money.

The pair's other printing (009) charges cards from hand instead; setup puts
exactly one of the two on the board and act 1 lays the other out in the
arrangement.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.Drugstore_008 (drugstore_008) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted

newtype Drugstore_008 = Drugstore_008 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drugstore_008 :: LocationCard Drugstore_008
drugstore_008 = location Drugstore_008 Cards.drugstore_008 2 (PerPlayer 1)

instance HasAbilities Drugstore_008 where
  getAbilities (Drugstore_008 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "drugstore.resources"
      $ groupLimit PerRound
      $ restricted a 1 (Here <> NoCluesOnThis)
      $ FastAbility
      $ ResourceCost 3

instance RunMessage Drugstore_008 where
  runMessage msg l@(Drugstore_008 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      placeClues (attrs.ability 1) attrs n
      pure l
    _ -> Drugstore_008 <$> liftRunMessage msg attrs
