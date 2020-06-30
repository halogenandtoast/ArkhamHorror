module Arkham.Types
  ( HasClues(..)
  , HasDeck(..)
  , HasHand(..)
  , HasDiscard(..)
  , HasInPlay(..)
  , HasPlayer(..)
  , HasPlayers(..)
  , HasDoom(..)
  , HasUses(..)
  , HasCardCode(..)
  , HasLocations(..)
  , HasInvestigator(..)
  , HasResources(..)
  , HasStacks(..)
  , HasPhase(..)
  , HasDifficulty(..)
  , HasGameStateStep(..)
  , HasScenario(..)
  , HasChaosBag(..)
  , HasCurrentData(..)
  , HasSanityDamage(..)
  , HasHealthDamage(..)
  , HasEncounterDeck(..)
  , endedTurn
  , actions
  , gameState
  , enemies
  )
where

import Arkham.Types.Card
import Arkham.Types.ChaosToken
import Arkham.Types.Difficulty
import Arkham.Types.Enemy
import Arkham.Types.Game
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Scenario
import ClassyPrelude
import Control.Monad.Random
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Lens.Micro
import Lens.Micro.Extras

class HasChaosBag a where
  chaosBag :: Lens' a (NonEmpty ArkhamChaosToken)
  drawChaosToken :: (MonadRandom m) => a -> m ArkhamChaosToken
  drawChaosToken a = let bag = a ^. chaosBag in (bag NE.!!) <$> getRandomR (0, NE.length bag - 1)

instance HasChaosBag ArkhamGameState where
  chaosBag = lens agsChaosBag $ \m x -> m { agsChaosBag = x }

instance HasChaosBag ArkhamGameData where
  chaosBag = gameState . chaosBag

class HasLocations a where
  locations :: Lens' a (HashMap ArkhamCardCode ArkhamLocation)

instance HasLocations ArkhamGameState where
  locations = lens agsLocations $ \m x -> m { agsLocations = x }

instance HasLocations ArkhamGameData where
  locations = gameState . locations

class HasCurrentData a where
  currentData :: Lens' a ArkhamGameData

class HasGameStateStep a where
  gameStateStep :: Lens' a ArkhamGameStateStep

instance HasGameStateStep ArkhamGameData where
  gameStateStep = gameState . gameStateStep

instance HasGameStateStep ArkhamGameState where
  gameStateStep = lens agsStep $ \m x -> m { agsStep = x }

class HasPlayer a where
  player :: Lens' a ArkhamPlayer

instance HasPlayer ArkhamGameData where
  player = gameState . player

instance HasPlayer ArkhamGameState where
  player = lens agsPlayer $ \m x -> m { agsPlayer = x }

class HasResources a where
  resources :: Lens' a Int

instance HasResources ArkhamPlayer where
  resources = lens _resources $ \m x -> m { _resources = x }

class HasPlayers a where
  players :: Lens' a [ArkhamInvestigator]

instance HasPlayers ArkhamGameState where
  players = lens (pure . view (player . investigator)) const

class HasInvestigator a where
  investigator :: Lens' a ArkhamInvestigator

instance HasInvestigator ArkhamPlayer where
  investigator = lens _investigator $ \m x -> m { _investigator = x }

class HasClues a where
  clues :: Lens' a Int

instance HasClues ArkhamLocation where
  clues = lens alClues $ \m x -> m { alClues = x }

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

class HasSanityDamage a where
  sanityDamage :: Lens' a Int

instance HasSanityDamage ArkhamPlayer where
  sanityDamage = lens _sanityDamage $ \m x -> m { _sanityDamage = x }

-- TODO: should this be combined with sanity to @HasDamage@
class HasHealthDamage a where
  healthDamage :: Lens' a Int

instance HasHealthDamage ArkhamPlayer where
  healthDamage = lens _healthDamage $ \m x -> m { _healthDamage = x }

class HasScenario a where
  scenario :: Lens' a ArkhamScenario

instance HasScenario ArkhamGameData where
  scenario = lens agScenario $ \m x -> m { agScenario = x }

class HasDifficulty a where
  difficulty :: Lens' a ArkhamDifficulty

instance HasDifficulty ArkhamGameData where
  difficulty = lens agDifficulty $ \m x -> m { agDifficulty = x }

class HasDoom a where
  doom :: Lens' a Int

instance HasDoom ArkhamStack where
  doom f (AgendaStack a) = AgendaStack <$> doom f a
  doom f (ActStack a) = ActStack a <$ f 0

_top :: Lens' (NonEmpty a) a
_top f (a :| as) = (:| as) <$> f a

instance HasDoom (NonEmpty ArkhamAgenda) where
  doom = _top . doom

instance HasDoom ArkhamAgenda where
  doom = lens aagendaDoom $ \m x -> m { aagendaDoom = x }

instance HasDoom ArkhamLocation where
  doom = lens alDoom $ \m x -> m { alDoom = x }

actions :: Lens' ArkhamPlayer Int
actions = lens _actionsRemaining $ \m x -> m { _actionsRemaining = x }

class HasPhase a where
  phase :: Lens' a ArkhamPhase

instance HasPhase ArkhamGameData where
  phase = gameState . phase

instance HasPhase ArkhamGameState where
  phase = lens agsPhase $ \m x -> m { agsPhase = x }

class HasStacks a where
  stacks :: Lens' a (HashMap Text ArkhamStack)

instance HasStacks ArkhamGameData where
  stacks = gameState . stacks

instance HasStacks ArkhamGameState where
  stacks = lens agsStacks $ \m x -> m { agsStacks = x }

class HasEncounterDeck a where
  encounterDeck :: Lens' a [ArkhamEncounterCard]

instance HasEncounterDeck ArkhamGameData where
  encounterDeck = gameState . encounterDeck

instance HasEncounterDeck ArkhamGameState where
  encounterDeck = lens agsEncounterDeck $ \m x -> m { agsEncounterDeck = x }

endedTurn :: Lens' ArkhamPlayer Bool
endedTurn = lens _endedTurn $ \m x -> m { _endedTurn = x }

enemies :: Lens' ArkhamLocation [ArkhamEnemy]
enemies = lens alEnemies $ \m x -> m { alEnemies = x }
