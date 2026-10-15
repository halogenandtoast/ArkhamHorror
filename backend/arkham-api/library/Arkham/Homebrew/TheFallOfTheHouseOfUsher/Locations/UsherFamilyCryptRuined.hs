module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.UsherFamilyCryptRuined (
  usherFamilyCryptRuined,
) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype UsherFamilyCryptRuined = UsherFamilyCryptRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

usherFamilyCryptRuined :: LocationCard UsherFamilyCryptRuined
usherFamilyCryptRuined = location UsherFamilyCryptRuined Cards.usherFamilyCryptRuined 6 (Static 0)

instance HasAbilities UsherFamilyCryptRuined where
  getAbilities (UsherFamilyCryptRuined a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "usherFamilyCryptRuined.omenOfDeath"
      $ restricted a 1 (DuringSkillTest $ SkillTestAt (be a))
      $ forced
      $ RevealChaosToken #when You
      $ mapOneOf ChaosTokenFaceIs [ElderSign, PlusOne, Zero]

instance RunMessage UsherFamilyCryptRuined where
  runMessage msg l@(UsherFamilyCryptRuined attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) token (ChaosTokenFaceModifier [Skull])
      pure l
    _ -> UsherFamilyCryptRuined <$> liftRunMessage msg attrs
