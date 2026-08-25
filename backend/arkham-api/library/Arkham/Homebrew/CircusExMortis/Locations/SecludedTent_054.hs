module Arkham.Homebrew.CircusExMortis.Locations.SecludedTent_054 (secludedTent_054) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
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

instance RunMessage SecludedTent_054 where
  runMessage msg l@(SecludedTent_054 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest $ skillTestAutomaticallySucceeds (attrs.ability 1)
      pure l
    _ -> SecludedTent_054 <$> liftRunMessage msg attrs
