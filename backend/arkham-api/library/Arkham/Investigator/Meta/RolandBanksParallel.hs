module Arkham.Investigator.Meta.RolandBanksParallel where

import Arkham.Name
import Arkham.Prelude

data RolandBanksParallelMeta = RolandBanksParallelMeta
  { dueDiligence :: Int -- fights
  , redTape :: Int -- plays
  , seekTheTruth :: Bool
  , leaveNoDoubt :: Int -- moves
  , ignoredDirectives :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultMeta :: RolandBanksParallelMeta
defaultMeta =
  RolandBanksParallelMeta
    { dueDiligence = 0
    , redTape = 0
    , seekTheTruth = True
    , leaveNoDoubt = 0
    , ignoredDirectives = []
    }

directiveNameToLabel :: Name -> Text
directiveNameToLabel name = case nameSubtitle name of
  Just "Due Diligence" -> "dueDiligence"
  Just "Red Tape" -> "redTape"
  Just "Consult Experts" -> "consultExperts"
  Just "Seek the Truth" -> "seekTheTruth"
  Just "Leave No Doubt" -> "leaveNoDoubt"
  _ -> error "invalid directive"
