module Arkham.Internal.Location
  ( allLocations
  , initLocation
  , lookupLocationInternal
  , ArkhamLocationInternal(..)
  , toLocationInternal
  )
where

import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Player
import Arkham.Types.Location
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe (fromJustNote)

data ClueValue = PerInvestigator Int | Static Int

cluesFor :: ClueValue -> Int -> Int
cluesFor (Static n) _ = n
cluesFor (PerInvestigator n) m = n * m

data ArkhamLocationInternal = ArkhamLocationInternal
  { aliCardCode :: ArkhamCardCode
  , aliOnReveal :: ArkhamGameState -> ArkhamLocation -> ArkhamLocation
  , aliOnEnter :: forall m. MonadIO m => ArkhamGameState -> ArkhamPlayer -> m (ArkhamGameState, ArkhamPlayer)
  , aliCanEnter :: ArkhamGameState -> Bool
  , aliName :: Text
  , aliLocationSymbol :: Maybe ArkhamLocationSymbol
  , aliConnectedLocationSymbols :: [ArkhamLocationSymbol]
  , aliUnrevealedLocationSymbol :: Maybe ArkhamLocationSymbol
  , aliUnrevealedConnectedLocationSymbols :: [ArkhamLocationSymbol]
  , aliBaseShroud :: Int
  }

initLocation :: ArkhamCardCode -> ArkhamLocation
initLocation code' = let internal = lookupLocationInternal code'
  in ArkhamLocation
    { alName = aliName internal
    , alCardCode = code'
    , alLocationSymbol = aliUnrevealedLocationSymbol internal
    , alConnectedLocationSymbols = aliUnrevealedConnectedLocationSymbols internal
    , alShroud = aliBaseShroud internal
    , alImage = "https://arkhamdb.com/bundles/cards/" <> unArkhamCardCode code' <> "b.png"
    , alInvestigators = []
    , alEnemies = []
    , alClues = 0
    , alDoom = 0
    , alStatus = Unrevealed
    }

toLocationInternal :: ArkhamLocation -> ArkhamLocationInternal
toLocationInternal l =
  fromJustNote ("Unkown internal location" <> show l)
    $ HashMap.lookup (alCardCode l) allLocations

lookupLocationInternal :: ArkhamCardCode -> ArkhamLocationInternal
lookupLocationInternal = fromJustNote "Could not find location"
  . flip HashMap.lookup allLocations 

allLocations :: HashMap ArkhamCardCode ArkhamLocationInternal
allLocations = HashMap.fromList $ map (\l -> (aliCardCode l, l))
  [ study
  , hallway
  , cellar
  , attic
  , parlor
  ]

defaultLocation :: ArkhamCardCode -> Text -> ClueValue -> ArkhamLocationInternal
defaultLocation code' name cv = ArkhamLocationInternal
  { aliName = name
  , aliCardCode = code'
  , aliLocationSymbol = Nothing
  , aliConnectedLocationSymbols = []
  , aliCanEnter = const True
  , aliOnReveal = \g l -> l { alStatus = Revealed } & clues .~ cluesFor cv (length $ g ^. players)
  , aliOnEnter = curry pure
  , aliBaseShroud = 0
  , aliUnrevealedLocationSymbol = Nothing
  , aliUnrevealedConnectedLocationSymbols = []
  }

study :: ArkhamLocationInternal
study = defaultLocation (ArkhamCardCode "01111") "Study" (PerInvestigator 2)

hallway :: ArkhamLocationInternal
hallway = defaultLocation (ArkhamCardCode "01112") "Hallway" (Static 0)

cellar :: ArkhamLocationInternal
cellar = (defaultLocation (ArkhamCardCode "01114") "Cellar" (PerInvestigator 2))
  { aliOnEnter = \g i -> pure (g, i & healthDamage +~ 1)
  }

attic :: ArkhamLocationInternal
attic = (defaultLocation (ArkhamCardCode "01113") "Attic" (PerInvestigator 2))
  { aliOnEnter = \g i -> pure (g, i & sanityDamage +~ 1)
  }

parlor :: ArkhamLocationInternal
parlor = (defaultLocation (ArkhamCardCode "01115") "Parloc" (Static 0))
  { aliCanEnter = const False
  }
