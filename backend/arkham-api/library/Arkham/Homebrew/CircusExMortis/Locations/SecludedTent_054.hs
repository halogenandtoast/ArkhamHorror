module Arkham.Homebrew.CircusExMortis.Locations.SecludedTent_054 (secludedTent_054) where

import Arkham.Ability
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype SecludedTent_054 = SecludedTent_054 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secludedTent_054 :: LocationCard SecludedTent_054
secludedTent_054 = location SecludedTent_054 Cards.secludedTent_054 4 (Static 2)

instance HasAbilities SecludedTent_054 where
  getAbilities (SecludedTent_054 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction (RevealChaosToken #when You moonToken)

{- | Ruling: you still seal the ☾ token, but stop drawing. Passing from the
reveal window ends the test before ST.4, which is what stops the draw — so the
seal half of the token's "seal this token and reveal another" has to be done
here, before the test ends and the engine's own resolution is skipped.
-}
instance RunMessage SecludedTent_054 where
  runMessage msg l@(SecludedTent_054 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      sealChaosToken iid iid token
      passSkillTest
      pure l
    _ -> SecludedTent_054 <$> liftRunMessage msg attrs
