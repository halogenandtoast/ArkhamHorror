module Arkham.Location.Cards.TheLittleBookshop (theLittleBookshop, TheLittleBookshop (..)) where

import Arkham.Ability
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype TheLittleBookshop = TheLittleBookshop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLittleBookshop :: LocationCard TheLittleBookshop
theLittleBookshop = location TheLittleBookshop Cards.theLittleBookshop 2 (PerPlayer 2)

instance HasAbilities TheLittleBookshop where
  getAbilities (TheLittleBookshop a) =
    extendRevealed1 a
      $ forcedAbility a 1
      $ SkillTestResult #after You (whileInvestigating a) #failure

instance RunMessage TheLittleBookshop where
  runMessage msg l@(TheLittleBookshop attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      shuffleIntoLeadsDeck . take 1 . unDeck =<< getEncounterDeck
      pure l
    _ -> TheLittleBookshop <$> liftRunMessage msg attrs
