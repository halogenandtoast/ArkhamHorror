module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.UsherFamilyCrypt (usherFamilyCrypt) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype UsherFamilyCrypt = UsherFamilyCrypt LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

usherFamilyCrypt :: LocationCard UsherFamilyCrypt
usherFamilyCrypt = location UsherFamilyCrypt Cards.usherFamilyCrypt 6 (PerPlayer 1)

instance HasAbilities UsherFamilyCrypt where
  getAbilities (UsherFamilyCrypt a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "usherFamilyCrypt.switchSign"
          $ restricted a 1 (DuringSkillTest $ SkillTestAt (be a))
          $ freeReaction (RevealChaosToken #when You #skull)
      , scenarioI18n $ collapseForced "usherFamilyCrypt.collapse" a 2
      ]

instance RunMessage UsherFamilyCrypt where
  runMessage msg l@(UsherFamilyCrypt attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) token NegativeToPositive
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      flipToRuined attrs
      pure l
    _ -> UsherFamilyCrypt <$> liftRunMessage msg attrs
