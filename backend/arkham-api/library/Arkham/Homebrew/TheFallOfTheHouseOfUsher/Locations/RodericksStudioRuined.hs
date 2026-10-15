module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.RodericksStudioRuined (rodericksStudioRuined) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RodericksStudioRuined = RodericksStudioRuined LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rodericksStudioRuined :: LocationCard RodericksStudioRuined
rodericksStudioRuined = location RodericksStudioRuined Cards.rodericksStudioRuined 4 (Static 0)

instance HasModifiersFor RodericksStudioRuined where
  getModifiersFor (RodericksStudioRuined a) = whenRevealed a $ hereGets a [ControlledAssetsCannotReady]

instance HasAbilities RodericksStudioRuined where
  getAbilities (RodericksStudioRuined a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "rodericksStudioRuined.gainVictory"
      $ groupLimit PerGame
      $ fastAbility a 1 (GroupClueCost (PerPlayer 1) Anywhere) Here

instance RunMessage RodericksStudioRuined where
  runMessage msg l@(RodericksStudioRuined attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs =<< perPlayer 1
      gameModifiers (attrs.ability 1) (toCard attrs) [GainVictory 1]
      pure l
    _ -> RodericksStudioRuined <$> liftRunMessage msg attrs
