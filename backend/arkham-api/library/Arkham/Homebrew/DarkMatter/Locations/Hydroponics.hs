module Arkham.Homebrew.DarkMatter.Locations.Hydroponics (hydroponics) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Hydroponics = Hydroponics LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hydroponics :: LocationCard Hydroponics
hydroponics = location Hydroponics Cards.hydroponics 3 (PerPlayer 2)

{- | "[free] If there are no clues on Hydroponics and you have fewer than 3 sanity
remaining: Each investigator at this location adds 1 tally mark next to their
'Memories'. (Group limit once per game.)"
-}
instance HasAbilities Hydroponics where
  getAbilities (Hydroponics a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> thisExists a LocationWithoutClues
            <> youExist (InvestigatorWithRemainingSanity $ LessThan $ Static 3)
        )
      $ FastAbility Free

instance RunMessage Hydroponics where
  runMessage msg l@(Hydroponics attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      here <- select $ investigatorAt attrs.id
      for_ here (`addMemories` 1)
      pure l
    _ -> Hydroponics <$> liftRunMessage msg attrs
