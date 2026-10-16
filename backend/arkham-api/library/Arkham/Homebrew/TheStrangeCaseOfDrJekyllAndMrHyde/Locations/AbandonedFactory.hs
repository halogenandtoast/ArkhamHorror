{- | Abandoned Factory -- the enemies here would rather be left alone.

"Gains aloof" is constant text, so it is a modifier and not an ability; it is
gated on 'whenRevealed' because 'HasModifiersFor' also runs while the location
is still on its back.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.AbandonedFactory (abandonedFactory) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AbandonedFactory = AbandonedFactory LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedFactory :: LocationCard AbandonedFactory
abandonedFactory = location AbandonedFactory Cards.abandonedFactory 5 (PerPlayer 1)

instance HasModifiersFor AbandonedFactory where
  getModifiersFor (AbandonedFactory a) =
    whenRevealed a
      $ modifySelect a (NonEliteEnemy <> enemyAt a.id) [AddKeyword Keyword.Aloof]

instance HasAbilities AbandonedFactory where
  getAbilities (AbandonedFactory a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "abandonedFactory.clue"
      $ groupLimit PerRound
      $ restricted a 1 (Here <> canDiscoverCluesAt (be a))
      $ freeReaction
      $ EnemyEngaged #after You (enemyAt a.id)

instance RunMessage AbandonedFactory where
  runMessage msg l@(AbandonedFactory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> AbandonedFactory <$> liftRunMessage msg attrs
