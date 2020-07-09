module Arkham.Internal.Agenda
  ( ArkhamAgendaInternal(..)
  , allAgendas
  , toInternalAgenda
  , lookupAgenda
  )
where

import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Player
import Base.Lock
import Base.Util
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Lens.Micro
import Lens.Micro.Extras
import Safe (fromJustNote)
import System.Random

data ArkhamAgendaInternal = ArkhamAgendaInternal
  { agendaSequence :: Text
  , agendaName :: Text
  , agendaCardCode :: ArkhamCardCode
  , agendaWillProgress :: ArkhamGameState -> Bool
  , agendaOnProgress :: forall m. MonadIO m => ArkhamAgenda -> ArkhamGameState -> m ArkhamGameState
  , agendaOnProgressLock :: Maybe (Lock ArkhamGameState)
  , agendaOnChoice :: forall m. MonadIO m => Int -> ArkhamGameState -> m ArkhamGameState
  }

toInternalAgenda :: ArkhamAgenda -> ArkhamAgendaInternal
toInternalAgenda ArkhamAgenda {..} = lookupAgenda aagendaCardCode

lookupAgenda :: ArkhamCardCode -> ArkhamAgendaInternal
lookupAgenda c =
  fromJustNote
      ("Could not find agenda for card code " <> unpack (unArkhamCardCode c))
    $ HashMap.lookup c allAgendas

allAgendas :: HashMap ArkhamCardCode ArkhamAgendaInternal
allAgendas = HashMap.fromList $ map
  (\a -> (agendaCardCode a, a))
  [whatsGoingOn, riseOfTheGhouls, theyreGettingOut]

totalDoom :: ArkhamGameState -> Int
totalDoom g = doomOnStacks + doomOnLocations
 where
  doomOnStacks = sum $ map (view doom) $ agsStacks g
  doomOnLocations = sum $ map (view doom) $ HashMap.elems $ agsLocations g

agenda :: Text -> Text -> ArkhamCardCode -> Int -> ArkhamAgendaInternal
agenda name sequence' code' doom' = ArkhamAgendaInternal
  { agendaSequence = sequence'
  , agendaName = name
  , agendaWillProgress = \g -> totalDoom g >= doom'
  , agendaOnProgress = const pure
  , agendaOnProgressLock = Nothing
  , agendaCardCode = code'
  , agendaOnChoice = const pure
  }

whatsGoingOn :: ArkhamAgendaInternal
whatsGoingOn = (agenda "What's going on" "1a" "01105" 3)
  { agendaOnProgress = \a g ->
    pure $ g & gameStateStep .~ ArkhamGameStateStepChooseOneStep
      (ArkhamChooseOneStep
        { acosPlayerId = _playerId $ g ^. activePlayer
        , acosChoices =
          [ "Each investigator discards 1 card at random from his or her hand"
          , "The lead investigator takes 2 horror"
          ]
        , acosChoiceTarget = AgendaTarget a
        }
      )
  , agendaOnProgressLock = Just (ChooseOne :| [])
  , agendaOnChoice = \c g -> case c of
    1 -> traverseOf
      (players . each)
      (\p -> do
        n <- liftIO $ randomRIO (0, length (p ^. hand) - 1)
        pure $ p & hand %~ without n & discard %~ ((p ^?! hand . ix n) :)
      )
      g
    2 -> pure $ g & leadInvestigator . sanityDamage +~ 2
    _ -> error "what have you done"
  }

leadInvestigator :: HasActivePlayer a => Lens' a ArkhamPlayer
leadInvestigator = activePlayer

riseOfTheGhouls :: ArkhamAgendaInternal
riseOfTheGhouls = agenda "Rise of the Ghouls" "2a" "01106" 7

theyreGettingOut :: ArkhamAgendaInternal
theyreGettingOut = agenda "They're getting out" "3a" "01107" 10
