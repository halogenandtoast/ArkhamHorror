{- | Shared "Arriving at <City>" reset, common to the back of every act in
this scenario except The Great Train Horror. Each act back reads, in order:
for each enemy in play place 1 doom on the agenda, read a "Now Arriving"
interlude, sweep all doom and enemies, cycle the Freight/Special car pool,
replenish Locomotive Engine and Caboose, then bring a Dark Young aboard.
-}
module Arkham.Homebrew.CircusExMortis.Acts.ArrivingAt (arrivingAt, putCircusTrainIntoPlay) where

import Arkham.Act.Import.Lifted
import Arkham.Helpers.Doom (targetsWithDoom)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.NowArriving
import Arkham.Homebrew.CircusExMortis.Traits
import Arkham.Matcher
import Arkham.Trait

replaceCarPool :: ReverseQueue m => Trait -> m ()
replaceCarPool trait = do
  whenJustM (selectOne $ LocationWithTrait trait) removeLocation
  replacement <- nonEmpty <$> getSetAsideCardsMatching (#location <> CardWithTrait trait)
  for_ replacement \cards -> do
    card <- sample cards
    lid <- placeLocation card
    reveal lid

spawnDarkYoungAt :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> m ()
spawnDarkYoungAt location = do
  remaining <- nonEmpty <$> getSetAsideCardsMatching (#enemy <> CardWithTrait DarkYoung)
  for_ remaining \cards -> do
    card <- sample cards
    createEnemyAt_ card location

{- | Places and reveals all three set-aside Circus Train locations, returning
Exotic Animal Car's freshly-minted id (the Dark Young's destination) directly
rather than re-querying for it, since the placement is only queued at this
point and not yet reflected in game state.
-}
putCircusTrainIntoPlay :: ReverseQueue m => m LocationId
putCircusTrainIntoPlay = do
  exoticAnimalCar <- placeSetAsideLocation Locations.exoticAnimalCar
  reveal exoticAnimalCar
  for_ [Locations.circusEngine, Locations.performersCar] (placeSetAsideLocation >=> reveal)
  pure exoticAnimalCar

arrivingAt :: (ReverseQueue m, Sourceable source) => source -> Arrival -> m LocationId -> m ()
arrivingAt source arrival resolveDarkYoungDestination = do
  enemyCount <- selectCount AnyEnemy
  placeDoomOnAgenda enemyCount
  nowArriving arrival
  traverse_ (removeAllDoom source) =<< targetsWithDoom
  selectEach AnyEnemy (toDiscard source)
  replaceCarPool FreightCar
  replaceCarPool SpecialCar
  selectEach (locationIs Locations.locomotiveEngine) (placeCluesUpToClueValue source)
  selectEach (locationIs Locations.caboose) (placeCluesUpToClueValue source)
  resolveDarkYoungDestination >>= spawnDarkYoungAt
