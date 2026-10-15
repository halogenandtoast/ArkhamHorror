module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.WineCellar (wineCellar) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WineCellar = WineCellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wineCellar :: LocationCard WineCellar
wineCellar = location WineCellar Cards.wineCellar 3 (PerPlayer 1)

instance HasAbilities WineCellar where
  getAbilities (WineCellar a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "wineCellar.fortifyingDraught"
          $ playerLimit PerRound
          $ restricted a 1 (Here <> DuringSkillTest (SkillTestOfInvestigator You))
          $ FastAbility Free
      , scenarioI18n $ collapseForced "wineCellar.collapse" a 2
      ]

instance RunMessage WineCellar where
  runMessage msg l@(WineCellar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      afterSkillTestQuiet
        $ roundModifiers (attrs.ability 1) iid [SkillModifier sk (-1) | sk <- [minBound ..]]
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      flipToRuined attrs
      pure l
    _ -> WineCellar <$> liftRunMessage msg attrs
