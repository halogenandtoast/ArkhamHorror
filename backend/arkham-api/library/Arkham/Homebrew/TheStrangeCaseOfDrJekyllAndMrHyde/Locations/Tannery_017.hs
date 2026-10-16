{- | Tannery (017) -- bleed for the icons, in horror.

See 'Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.Tannery_016':
the printed @{fast}@ is a reaction on 'CommittedCard' because only the window
names the card whose icons double.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.Tannery_017 (tannery_017) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getCommittedCard)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Tannery_017 = Tannery_017 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tannery_017 :: LocationCard Tannery_017
tannery_017 = location Tannery_017 Cards.tannery_017 3 (PerPlayer 2)

instance HasAbilities Tannery_017 where
  getAbilities (Tannery_017 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "tannery.horror"
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ triggered (CommittedCard #when You #any) (HorrorCost (a.ability 1) YouTarget 1)

instance RunMessage Tannery_017 where
  runMessage msg l@(Tannery_017 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card DoubleSkillIcons
      pure l
    _ -> Tannery_017 <$> liftRunMessage msg attrs
