module Arkham.Internal.Agenda where

import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro.Extras

data ArkhamAgendaInternal = ArkhamAgendaInternal
  { agendaSequence :: Text
  , agendaWillProgress :: ArkhamGameState -> Bool
  , agendaOnProgress :: ArkhamGameState -> ArkhamGameState
  }

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
  }
