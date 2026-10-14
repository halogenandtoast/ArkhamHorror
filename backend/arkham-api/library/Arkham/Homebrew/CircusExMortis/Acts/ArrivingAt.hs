{- | Shared "Arriving at <City>" reset, common to the back of every act in
this scenario except The Great Train Horror. Each act back reads, in order:
for each enemy in play place 1 doom on the agenda, read a "Now Arriving"
interlude, sweep all doom and enemies, cycle the Freight/Special car pool
(banking each replaced car with Victory X and no clues), replenish Locomotive
Engine and Caboose, then bring a Dark Young aboard.
-}
module Arkham.Homebrew.CircusExMortis.Acts.ArrivingAt (arrivingAt, putCircusTrainIntoPlay) where

import Arkham.Act.Import.Lifted
import Arkham.Helpers.Card (getHasVictoryPoints)
import Arkham.Helpers.Doom (targetsWithDoom)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.NowArriving
import Arkham.Homebrew.CircusExMortis.Traits
import Arkham.Location.Types (Field (LocationCard))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Projection
import Arkham.Trait

-- The car is swapped out, not torn off the train: investigators standing on it
-- ride along to its replacement, so this replaces the location in place rather
-- than removing it (removal leaves them unplaced and eliminates them).
replaceCarPool :: ReverseQueue m => Trait -> m ()
replaceCarPool trait = do
  replacement <- nonEmpty <$> getSetAsideCardsMatching (#location <> CardWithTrait trait)
  for_ replacement \cards -> do
    card <- sample cards
    selectOne (LocationWithTrait trait) >>= \case
      Just lid -> do
        bankReplacedCar lid
        push $ ReplaceLocation lid card DefaultReplace
        reveal lid
      Nothing -> reveal =<< placeLocation card

{- | "Move each replaced location with Victory X and no clues to the victory
display." Written against Victory X rather than the trait, as the act back is:
only the Special Cars print one, but that is the card's wording, not a
coincidence this should bake in.

Without it the swap is the card's last appearance -- 'ReplaceLocation'
overwrites the entity in place -- so four of the five Special Cars could never
be scored. The fifth only scored because 'generateXpReport' also counts a
cleared location still in play at the end.

Banked by card id rather than with 'addToVictory' on the location, which pushes
'RemoveLocation': the car has to stay in place for 'ReplaceLocation' to carry
anyone standing on it across.
-}
bankReplacedCar :: ReverseQueue m => LocationId -> m ()
bankReplacedCar lid = whenMatch lid LocationWithoutClues do
  card <- field LocationCard lid
  whenM (getHasVictoryPoints card) $ push $ AddToVictory Nothing (CardIdTarget card.id)

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
