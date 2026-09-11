module Arkham.Homebrew.CircusExMortis.Locations.SecludedTent_054 (secludedTent_054) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.SkillTest.Step

newtype SecludedTent_054 = SecludedTent_054 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secludedTent_054 :: LocationCard SecludedTent_054
secludedTent_054 = location SecludedTent_054 Cards.secludedTent_054 4 (Static 2)

{- | "This test automatically succeeds" applies at ST.6 (FAQ 2.9), so the ☾
token still seals and reveals another at ST.4. Triggering off the reveal window
would end the test before that, so the reaction sits on the after-ST.4 window
instead (same seam as Cryptic Grimoire (Text of the Elder Herald)).
-}
instance HasAbilities SecludedTent_054 where
  getAbilities (SecludedTent_054 a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringSkillTest (SkillTestWithResolvedChaosTokenBy You moonToken))
      $ freeReaction (SkillTestStep #after ResolveChaosSymbolEffectsStep)

instance RunMessage SecludedTent_054 where
  runMessage msg l@(SecludedTent_054 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      passSkillTest
      pure l
    _ -> SecludedTent_054 <$> liftRunMessage msg attrs
