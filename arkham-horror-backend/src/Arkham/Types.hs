module Arkham.Types
  ( module X
  , module Arkham.Types
  )
where

import Arkham.Entity.ArkhamGame as X
import Arkham.Types.Action as X
import Arkham.Types.Card as X
import Arkham.Types.ChaosToken as X
import Arkham.Types.Game as X
import Arkham.Types.GameState as X
import Arkham.Types.Investigator as X
import Arkham.Types.Location as X
import Arkham.Types.Player as X
import Arkham.Types.Scenario as X
import Arkham.Types.Skill as X
import ClassyPrelude
import Control.Monad.Random
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Lens.Micro

gameState :: Lens' ArkhamGameData ArkhamGameState
gameState = lens agGameState $ \m x -> m { agGameState = x }

class HasChaosBag a where
  chaosBag :: Lens' a (NonEmpty ArkhamChaosToken)
  drawChaosToken :: (MonadRandom m) => a -> m ArkhamChaosToken
  drawChaosToken a = let bag = a ^. chaosBag in (bag NE.!!) <$> getRandomR (0, NE.length bag - 1)

instance HasChaosBag ArkhamGameState where
  chaosBag = lens agsChaosBag $ \m x -> m { agsChaosBag = x }

instance HasChaosBag ArkhamGameData where
  chaosBag = gameState . chaosBag

instance HasChaosBag ArkhamGame where
  chaosBag = currentData . chaosBag

class HasLocations a where
  locations :: Lens' a (HashMap LocationId ArkhamLocation)

instance HasLocations ArkhamGameState where
  locations = lens agsLocations $ \m x -> m { agsLocations = x }

instance HasLocations ArkhamGameData where
  locations = gameState . locations

instance HasLocations ArkhamGame where
  locations = currentData . locations

class HasLocationId a where
  locationId :: Lens' a LocationId

instance HasLocationId ArkhamUnrevealedLocation where
  locationId = lens aulLocationId $ \m x -> m { aulLocationId = x }

instance HasLocationId ArkhamRevealedLocation where
  locationId = lens arlLocationId $ \m x -> m { arlLocationId = x }

instance HasLocationId ArkhamLocation where
  locationId f = \case
    RevealedLocation l -> RevealedLocation <$> locationId f l
    UnrevealedLocation l -> UnrevealedLocation <$> locationId f l

class HasCurrentData a where
  currentData :: Lens' a ArkhamGameData

instance HasCurrentData ArkhamGame where
  currentData =
    lens arkhamGameCurrentData (\m x -> m { arkhamGameCurrentData = x })

class HasGameStateStep a where
  gameStateStep :: Lens' a ArkhamGameStateStep

instance HasGameStateStep ArkhamGame where
  gameStateStep = currentData . gameStateStep

instance HasGameStateStep ArkhamGameData where
  gameStateStep = gameState . gameStateStep

instance HasGameStateStep ArkhamGameState where
  gameStateStep = lens agsStep $ \m x -> m { agsStep = x }

class HasPlayer a where
  player :: Lens' a ArkhamPlayer

instance HasPlayer ArkhamGame where
  player = currentData . player

instance HasPlayer ArkhamGameData where
  player = gameState . player

instance HasPlayer ArkhamGameState where
  player = lens agsPlayer $ \m x -> m { agsPlayer = x }

class HasResources a where
  resources :: Lens' a Int

instance HasResources ArkhamPlayer where
  resources = lens _resources $ \m x -> m { _resources = x }

class HasInvestigator a where
  investigator :: Lens' a ArkhamInvestigator

instance HasInvestigator ArkhamPlayer where
  investigator = lens _investigator $ \m x -> m { _investigator = x }

class HasClues a where
  clues :: Lens' a Int

isLocationClues :: LocationContent -> Bool
isLocationClues (LocationClues _) = True
isLocationClues _ = False

instance HasClues ArkhamRevealedLocation where
  clues =
    lens
        (\m ->
          fromMaybe 0 $ listToMaybe [ n | LocationClues n <- arlContents m ]
        )
      $ \m x -> m
          { arlContents =
            LocationClues x
              : [ c | c <- arlContents m, not (isLocationClues c) ]
          }

instance HasClues ArkhamUnrevealedLocation where
  clues =
    lens
        (\m ->
          fromMaybe 0 $ listToMaybe [ n | LocationClues n <- aulContents m ]
        )
      $ \m x -> m
          { aulContents =
            LocationClues x
              : [ c | c <- aulContents m, not (isLocationClues c) ]
          }

instance HasClues ArkhamLocation where
  clues f = \case
    RevealedLocation location -> RevealedLocation <$> clues f location
    UnrevealedLocation location -> UnrevealedLocation <$> clues f location

instance HasClues ArkhamPlayer where
  clues = lens _clues $ \m x -> m { _clues = x }

instance HasClues a => HasClues (Maybe a) where
  clues f = \case
    Nothing -> Nothing <$ f 0
    Just x -> Just <$> clues f x

class HasHand a where
  hand :: Lens' a [ArkhamCard]

instance HasHand ArkhamPlayer where
  hand = lens _hand $ \m x -> m { _hand = x }

class HasDeck a where
  deck :: Lens' a [ArkhamCard]

instance HasDeck ArkhamPlayer where
  deck = lens _deck $ \m x -> m { _deck = x }

class HasUses a where
  uses :: Lens' a (Maybe Int)

instance HasUses ArkhamCard where
  uses f = \case
    PlayerCard c -> PlayerCard <$> uses f c
    EncounterCard c -> EncounterCard <$> uses f c

instance HasUses ArkhamPlayerCard where
  uses = lens apcUses $ \m x -> m { apcUses = x }

instance HasUses ArkhamEncounterCard where
  uses = lens (const Nothing) const

class HasCardCode a where
  cardCode :: Lens' a ArkhamCardCode

instance HasCardCode ArkhamCard where
  cardCode f = \case
    PlayerCard c -> PlayerCard <$> cardCode f c
    EncounterCard c -> EncounterCard <$> cardCode f c

instance HasCardCode ArkhamPlayerCard where
  cardCode = lens apcCode $ \m x -> m { apcCode = x }

instance HasCardCode ArkhamEncounterCard where
  cardCode = lens aecCode $ \m x -> m { aecCode = x }

class HasInPlay a where
  inPlay :: Lens' a [ArkhamCard]

instance HasInPlay ArkhamPlayer where
  inPlay = lens _inPlay $ \m x -> m { _inPlay = x }

class HasDiscard a where
  discard :: Lens' a [ArkhamCard]

instance HasDiscard ArkhamPlayer where
  discard = lens _discard $ \m x -> m { _discard = x }

class HasLocationContents a where
  locationContents :: Lens' a [LocationContent]

instance HasLocationContents ArkhamLocation where
  locationContents f = \case
    RevealedLocation l -> RevealedLocation <$> locationContents f l
    UnrevealedLocation l -> UnrevealedLocation <$> locationContents f l

instance HasLocationContents ArkhamRevealedLocation where
  locationContents = lens arlContents $ \m x -> m { arlContents = x }

instance HasLocationContents ArkhamUnrevealedLocation where
  locationContents = lens aulContents $ \m x -> m { aulContents = x }

class HasSanityDamage a where
  sanityDamage :: Lens' a Int

instance HasSanityDamage ArkhamPlayer where
  sanityDamage = lens _sanityDamage $ \m x -> m { _sanityDamage = x }

-- TODO: should this be combined with sanity to @HasDamage@
class HasHealthDamage a where
  healthDamage :: Lens' a Int

instance HasHealthDamage ArkhamPlayer where
  healthDamage = lens _healthDamage $ \m x -> m { _healthDamage = x }
