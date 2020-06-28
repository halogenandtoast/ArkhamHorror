module Arkham.Types
  ( module X
  , module Arkham.Types
  )
where

import Arkham.Entity.ArkhamGame as X
import Arkham.Types.Action as X
import Arkham.Types.Card as X
import Arkham.Types.ChaosToken as X
import Arkham.Types.Difficulty as X
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
  locations :: Lens' a (HashMap ArkhamCardCode ArkhamLocation)

instance HasLocations ArkhamGameState where
  locations = lens agsLocations $ \m x -> m { agsLocations = x }

instance HasLocations ArkhamGameData where
  locations = gameState . locations

instance HasLocations ArkhamGame where
  locations = currentData . locations

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

instance HasScenario ArkhamGame where
  scenario = currentData . scenario

instance HasScenario ArkhamGameData where
  scenario = lens agScenario $ \m x -> m { agScenario = x }

class HasDifficulty a where
  difficulty :: Lens' a ArkhamDifficulty

instance HasDifficulty ArkhamGame where
  difficulty = currentData . difficulty

instance HasDifficulty ArkhamGameData where
  difficulty = lens agDifficulty $ \m x -> m { agDifficulty = x }

class HasDoom a where
  doom :: Lens' a Int

instance HasDoom ArkhamStack where
  doom f (AgendaStack a) = AgendaStack a <$ f 0
  doom f (ActStack a) = ActStack <$> doom f a

instance HasDoom ArkhamAct where
  doom = lens aactDoom $ \m x -> m { aactDoom = x }

instance HasDoom ArkhamLocation where
  doom = lens alDoom $ \m x -> m { alDoom = x }

actions :: Lens' ArkhamPlayer Int
actions = lens _actionsRemaining $ \m x -> m { _actionsRemaining = x }

class HasPhase a where
  phase :: Lens' a ArkhamPhase

instance HasPhase ArkhamGame where
  phase = currentData . phase

instance HasPhase ArkhamGameData where
  phase = gameState . phase

instance HasPhase ArkhamGameState where
  phase = lens agsPhase $ \m x -> m { agsPhase = x }

class HasStacks a where
  stacks :: Lens' a (HashMap Text ArkhamStack)

instance HasStacks ArkhamGame where
  stacks = currentData . stacks

instance HasStacks ArkhamGameData where
  stacks = gameState . stacks

instance HasStacks ArkhamGameState where
  stacks = lens agsStacks $ \m x -> m { agsStacks = x }

endedTurn :: Lens' ArkhamPlayer Bool
endedTurn = lens _endedTurn $ \m x -> m { _endedTurn = x }
