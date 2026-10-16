{- | Northside Plaza -- the square the investigators start in, and the way out.

The Forced clue comes "from the token bank", so it is a plain 'placeClues' and
not a transfer off anyone; the plaza prints no clue value of its own.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.NorthsidePlaza (northsidePlaza) where

import Arkham.Ability
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype NorthsidePlaza = NorthsidePlaza LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northsidePlaza :: LocationCard NorthsidePlaza
northsidePlaza = location NorthsidePlaza Cards.northsidePlaza 3 (Static 0)

instance HasAbilities NorthsidePlaza where
  getAbilities (NorthsidePlaza a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "northsidePlaza.resign" $ locationResignAction a
      , scenarioI18n
          $ withI18nTooltip "northsidePlaza.clue"
          $ restricted a 1 (exists $ NonEliteEnemy <> enemyAt a.id)
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage NorthsidePlaza where
  runMessage msg l@(NorthsidePlaza attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs 1
      pure l
    _ -> NorthsidePlaza <$> liftRunMessage msg attrs
