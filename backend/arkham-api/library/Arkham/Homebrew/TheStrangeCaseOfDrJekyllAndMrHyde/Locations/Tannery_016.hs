{- | Tannery (016) -- bleed for the icons.

The printed @{fast}@ reads off the commit, and only the window names the card
whose icons double, so this is a reaction on 'CommittedCard' -- a bare
'FastAbility' defaults to 'FastPlayerWindow' and could never see a commit. The
pair's other printing (017) charges horror instead.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.Tannery_016 (tannery_016) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getCommittedCard)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Tannery_016 = Tannery_016 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tannery_016 :: LocationCard Tannery_016
tannery_016 = location Tannery_016 Cards.tannery_016 3 (PerPlayer 2)

instance HasAbilities Tannery_016 where
  getAbilities (Tannery_016 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "tannery.damage"
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ triggered (CommittedCard #when You #any) (DamageCost (a.ability 1) YouTarget 1)

instance RunMessage Tannery_016 where
  runMessage msg l@(Tannery_016 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) card DoubleSkillIcons
      pure l
    _ -> Tannery_016 <$> liftRunMessage msg attrs
