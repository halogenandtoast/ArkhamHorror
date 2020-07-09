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
import Arkham.Util
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Lens.Micro.Extras
import Safe (fromJustNote)
import System.Random

data ArkhamAgendaInternal = ArkhamAgendaInternal
  { agendaSequence :: Text
  , agendaCardCode :: ArkhamCardCode
  , agendaWillProgress :: ArkhamGameState -> Bool
  , agendaOnProgress :: forall m. MonadIO m => ArkhamGameState -> ArkhamAgenda -> m ArkhamGameState
  , agendaOnChoice :: forall m. MonadIO m => ArkhamGameState -> Int -> m ArkhamGameState
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

whatsGoingOn :: ArkhamAgendaInternal
whatsGoingOn = ArkhamAgendaInternal
  { agendaSequence = "1a"
  , agendaWillProgress = \g -> totalDoom g > 3
  , agendaOnProgress = \g a ->
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
  , agendaCardCode = "01105"
  , agendaOnChoice = \g c -> case c of
    1 -> traverseOf
      (players . each . hand)
      (\h -> without <$> liftIO (randomRIO (0, length h - 1)) <*> pure h)
      g
    2 -> pure $ g & leadInvestigator . sanityDamage +~ 2
    _ -> error "what have you done"
  }

leadInvestigator :: HasActivePlayer a => Lens' a ArkhamPlayer
leadInvestigator = activePlayer

riseOfTheGhouls :: ArkhamAgendaInternal
riseOfTheGhouls = ArkhamAgendaInternal
  { agendaSequence = "2a"
  , agendaWillProgress = \g -> totalDoom g > 7
  , agendaOnProgress = const . pure
  , agendaCardCode = "01106"
  , agendaOnChoice = \g _ -> pure g
  }

theyreGettingOut :: ArkhamAgendaInternal
theyreGettingOut = ArkhamAgendaInternal
  { agendaSequence = "3a"
  , agendaWillProgress = \g -> totalDoom g > 10
  , agendaOnProgress = const . pure
  , agendaCardCode = "01107"
  , agendaOnChoice = \g _ -> pure g
  }
