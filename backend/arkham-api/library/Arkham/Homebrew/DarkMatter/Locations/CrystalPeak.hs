module Arkham.Homebrew.DarkMatter.Locations.CrystalPeak (crystalPeak) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (shuffleIntoScanningDeck)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Token qualified as Token

newtype CrystalPeak = CrystalPeak LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPeak :: LocationCard CrystalPeak
crystalPeak = symbolLabel $ location CrystalPeak Cards.crystalPeak 3 (PerPlayer 2)

{- | "Forced - At the end of the round, if this location is empty and does not
have a resource token on it: Shuffle it back into the scanning deck.
[free] If there are no clues on this location: Record in your Campaign Log that
you have witnessed the primordial chaos. (Max once per game.)"
-}
instance HasAbilities CrystalPeak where
  getAbilities (CrystalPeak a) =
    extendRevealed
      a
      [ restricted
          a
          1
          ( thisExists a
              $ LocationWithoutInvestigators
              <> LocationWithoutEnemies
              <> not_ (LocationWithToken Token.Resource)
          )
          $ forced
          $ RoundEnds #when
      , groupLimit PerGame
          $ restricted a 2 (Here <> thisExists a LocationWithoutClues)
          $ FastAbility Free
      ]

instance RunMessage CrystalPeak where
  runMessage msg l@(CrystalPeak attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      shuffleIntoScanningDeck [toCard attrs]
      push $ RemoveLocation attrs.id
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      record YouHaveWitnessedThePrimordialChaos
      pure l
    _ -> CrystalPeak <$> liftRunMessage msg attrs
