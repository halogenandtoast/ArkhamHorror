{- | Etchers' Shop (012) -- buy a clue with money.

'canDiscoverCluesAt' is the one discovery predicate: without it the ability is
offered with no clue left to find and the 3 resources buy nothing.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.EtchersShop_012 (etchersShop_012) where

import Arkham.Ability
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EtchersShop_012 = EtchersShop_012 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etchersShop_012 :: LocationCard EtchersShop_012
etchersShop_012 = location EtchersShop_012 Cards.etchersShop_012 4 (PerPlayer 2)

instance HasAbilities EtchersShop_012 where
  getAbilities (EtchersShop_012 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "etchersShop.resources"
      $ restricted a 1 (Here <> canDiscoverCluesAt (be a))
      $ actionAbilityWithCost (ResourceCost 3)

instance RunMessage EtchersShop_012 where
  runMessage msg l@(EtchersShop_012 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> EtchersShop_012 <$> liftRunMessage msg attrs
