module Arkham.Homebrew.DarkMatter.Locations.Earth (earth) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Earth = Earth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earth :: LocationCard Earth
earth = location Earth Cards.earth 3 (PerPlayer 2)

-- | "Forced - After you enter this location: You are immediately driven insane."
instance HasAbilities Earth where
  getAbilities (Earth a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ Enters #after You (be a)
      , mkAbility a 2 $ forced $ RevealLocation #after Anyone (be a)
      ]

instance RunMessage Earth where
  runMessage msg l@(Earth attrs) = runQueueT $ case msg of
    {- "Revelation - Put this location into play. Check your Campaign Log. Each
    investigator who has been corrupted by the Earth takes 2 damage." The log
    records corruption for the whole group. -}
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      corrupted <- getHasRecord AllInvestigatorsHaveBeenCorruptedByTheEarth
      when corrupted $ eachInvestigator \iid -> assignDamage iid (attrs.ability 2) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrivenInsane iid
      pure l
    _ -> Earth <$> liftRunMessage msg attrs
