module Arkham.Location.Cards.ExhibitHallTheArchives (exhibitHallTheArchives) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ExhibitHallTheArchives = ExhibitHallTheArchives LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallTheArchives :: LocationCard ExhibitHallTheArchives
exhibitHallTheArchives = location ExhibitHallTheArchives Cards.exhibitHallTheArchives 5 (Static 0)

instance HasAbilities ExhibitHallTheArchives where
  getAbilities (ExhibitHallTheArchives a) =
    extendRevealed1 a $ restricted a 1 (Here <> youExist can.gain.clues) doubleActionAbility

instance RunMessage ExhibitHallTheArchives where
  runMessage msg l@(ExhibitHallTheArchives attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      mHuntingHorror <- getInPlayHuntingHorror
      case mHuntingHorror of
        Just _ -> do
          mShadowSpawned <- selectOne $ treacheryIs Treacheries.shadowSpawned
          for_ mShadowSpawned \shadowSpawned -> placeTokens (attrs.ability 1) shadowSpawned #resource 1
        Nothing -> do
          findEncounterCardIn
            iid
            attrs
            (cardIs Enemies.huntingHorror)
            [FromEncounterDeck, FromEncounterDiscard, FromVoid]
      pure l
    FoundEnemyInOutOfPlay VoidZone _ (isTarget attrs -> True) eid -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      push $ EnemySpawnFromOutOfPlay VoidZone Nothing lid eid
      pure l
    FoundEncounterCard _ (isTarget attrs -> True) ec -> do
      lid <- selectJust $ LocationWithTitle "Museum Halls"
      spawnEnemyAt_ ec lid
      pure l
    _ -> ExhibitHallTheArchives <$> liftRunMessage msg attrs
