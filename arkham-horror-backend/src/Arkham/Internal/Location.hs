module Arkham.Internal.Location
  ( allLocations
  , ArkhamLocationInternal(..)
  , toLocationInternal
  )
where

import Arkham.Types
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe (fromJustNote)

data ArkhamLocationInternal = ArkhamLocationInternal
  { aliCardCode :: ArkhamCardCode
  , aliOnReveal :: ArkhamGameState -> ArkhamLocation -> ArkhamLocation
  }

toLocationInternal :: ArkhamLocation -> ArkhamLocationInternal
toLocationInternal l =
  fromJustNote ("Unkown internal location" <> show l)
    $ HashMap.lookup (alCardCode l) allLocations

allLocations :: HashMap ArkhamCardCode ArkhamLocationInternal
allLocations = HashMap.fromList $ map (\l -> (aliCardCode l, l)) [study]

study :: ArkhamLocationInternal
study = ArkhamLocationInternal
  { aliCardCode = ArkhamCardCode "01111"
  , aliOnReveal = \g l ->
    (l { alStatus = Revealed }) & clues .~ (2 * length (g ^. players))
  }
