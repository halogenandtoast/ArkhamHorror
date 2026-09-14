module Arkham.Homebrew.DarkMatter.Locations.LandingCraft (landingCraft) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LandingCraft = LandingCraft LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

landingCraft :: LocationCard LandingCraft
landingCraft = symbolLabel $ location LandingCraft Cards.landingCraft 3 (Static 1)

{- | "[free]: Place 1 of your clues onto this location.
While there are 2[per_investigator] or more clues on this location, it gains
'[action] Resign. Escape this migraine-inducing asteroid.'"
-}
instance HasAbilities LandingCraft where
  getAbilities (LandingCraft a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist InvestigatorWithAnyClues) $ FastAbility Free
      , restrict (thisExists a $ LocationWithClues $ AtLeast $ PerPlayer 2)
          $ locationResignAction a
      ]

instance RunMessage LandingCraft where
  runMessage msg l@(LandingCraft attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) iid attrs #clue 1
      pure l
    _ -> LandingCraft <$> liftRunMessage msg attrs
