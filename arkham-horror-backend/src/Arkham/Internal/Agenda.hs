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
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro.Extras
import Safe (fromJustNote)

data ArkhamAgendaInternal = ArkhamAgendaInternal
  { agendaSequence :: Text
  , agendaCardCode :: ArkhamCardCode
  , agendaWillProgress :: ArkhamGameState -> Bool
  , agendaOnProgress :: ArkhamGameState -> ArkhamGameState
  }

toInternalAgenda :: ArkhamAgenda -> ArkhamAgendaInternal
toInternalAgenda ArkhamAgenda {..} = lookupAgenda aagendaCardCode

lookupAgenda :: ArkhamCardCode -> ArkhamAgendaInternal
lookupAgenda c =
  fromJustNote
      ("Could not find agenda for card code " <> unpack (unArkhamCardCode c))
    $ HashMap.lookup c allAgendas

allAgendas :: HashMap ArkhamCardCode ArkhamAgendaInternal
allAgendas =
  HashMap.fromList $ map (\a -> (agendaCardCode a, a)) [whatsGoingOn]

totalDoom :: ArkhamGameState -> Int
totalDoom g = doomOnStacks + doomOnLocations
 where
  doomOnStacks = sum $ map (view doom) $ agsStacks g
  doomOnLocations = sum $ map (view doom) $ HashMap.elems $ agsLocations g

whatsGoingOn :: ArkhamAgendaInternal
whatsGoingOn = ArkhamAgendaInternal
  { agendaSequence = "1a"
  , agendaWillProgress = \g -> totalDoom g > 3
  , agendaOnProgress = id
  , agendaCardCode = ArkhamCardCode "01105"
  }
