{- | Drugstore (009) -- buy the clues back with cards.

The pair's other printing (008) charges resources instead; setup puts exactly
one of the two on the board and act 1 lays the other out in the arrangement.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.Drugstore_009 (drugstore_009) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted

newtype Drugstore_009 = Drugstore_009 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drugstore_009 :: LocationCard Drugstore_009
drugstore_009 = location Drugstore_009 Cards.drugstore_009 2 (PerPlayer 1)

instance HasAbilities Drugstore_009 where
  getAbilities (Drugstore_009 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "drugstore.discard"
      $ groupLimit PerRound
      $ restricted a 1 (Here <> NoCluesOnThis)
      $ FastAbility
      $ HandDiscardCost 3 #any

instance RunMessage Drugstore_009 where
  runMessage msg l@(Drugstore_009 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      placeClues (attrs.ability 1) attrs n
      pure l
    _ -> Drugstore_009 <$> liftRunMessage msg attrs
