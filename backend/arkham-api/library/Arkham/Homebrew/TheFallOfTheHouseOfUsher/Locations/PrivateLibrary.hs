module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.PrivateLibrary (privateLibrary) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)

newtype PrivateLibrary = PrivateLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

privateLibrary :: LocationCard PrivateLibrary
privateLibrary = location PrivateLibrary Cards.privateLibrary 3 (PerPlayer 2)

instance HasAbilities PrivateLibrary where
  getAbilities (PrivateLibrary a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "privateLibrary.unearth"
          $ groupLimit PerGame
          $ restricted a 1 (Here <> thisExists a LocationWithoutClues) freeTrigger_
      , scenarioI18n $ collapseForced "privateLibrary.collapse" a 2
      ]

instance RunMessage PrivateLibrary where
  runMessage msg l@(PrivateLibrary attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember unearthedAnArchaicBook
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      flipToRuined attrs
      pure l
    _ -> PrivateLibrary <$> liftRunMessage msg attrs
