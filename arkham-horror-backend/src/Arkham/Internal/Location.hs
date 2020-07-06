module Arkham.Internal.Location
  ( allLocations
  , initLocation
  , lookupLocationInternal
  , ArkhamLocationInternal(..)
  , toLocationInternal
  , locationFor
  )
where

import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Types.Player
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe (fromJustNote)

locationFor :: ArkhamPlayer -> ArkhamGameState -> ArkhamLocation
locationFor p g =
  fromJustNote "the investigator appears to be nowhere"
    $ find (playerIsAtLocation p)
    $ HashMap.elems (g ^. locations)

playerIsAtLocation :: ArkhamPlayer -> ArkhamLocation -> Bool
playerIsAtLocation p = elem (_playerId p) . alInvestigators

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
initLocation code' =
  let internal = lookupLocationInternal code'
  in
    ArkhamLocation
      { alName = aliName internal
      , alCardCode = code'
      , alLocationSymbol = aliUnrevealedLocationSymbol internal
      , alConnectedLocationSymbols = aliUnrevealedConnectedLocationSymbols
        internal
      , alShroud = aliBaseShroud internal
      , alImage = locationImage code' Unrevealed
      , alInvestigators = mempty
      , alEnemies = mempty
      , alClues = 0
      , alDoom = 0
      , alStatus = Unrevealed
      }

toLocationInternal :: ArkhamLocation -> ArkhamLocationInternal
toLocationInternal l =
  fromJustNote ("Unkown internal location" <> show l)
    $ HashMap.lookup (alCardCode l) allLocations

lookupLocationInternal :: ArkhamCardCode -> ArkhamLocationInternal
lookupLocationInternal =
  fromJustNote "Could not find location" . flip HashMap.lookup allLocations

allLocations :: HashMap ArkhamCardCode ArkhamLocationInternal
allLocations = HashMap.fromList
  $ map (\l -> (aliCardCode l, l)) [study, hallway, cellar, attic, parlor]

locationImage :: ArkhamCardCode -> ArkhamLocationStatus -> Text
locationImage code' status' =
  "https://arkhamdb.com/bundles/cards/" <> unArkhamCardCode code' <> side
 where
  side = case (status', code') of
    (Revealed, "01111") -> ".png"
    (Revealed, _) -> ".jpg"
    _ -> "b.png"

defaultLocation
  :: ArkhamCardCode
  -> Text
  -> ClueValue
  -> Maybe ArkhamLocationSymbol
  -> [ArkhamLocationSymbol]
  -> ArkhamLocationInternal
defaultLocation code' name cv msym syms = ArkhamLocationInternal
  { aliName = name
  , aliCardCode = code'
  , aliLocationSymbol = msym
  , aliConnectedLocationSymbols = syms
  , aliCanEnter = const True
  , aliOnReveal = \g l ->
    l { alStatus = Revealed, alImage = locationImage code' Revealed }
      & clues
      .~ cluesFor cv (length $ g ^. players)
  , aliOnEnter = curry pure
  , aliBaseShroud = 0
  , aliUnrevealedLocationSymbol = msym
  , aliUnrevealedConnectedLocationSymbols = syms
  }

study :: ArkhamLocationInternal
study = defaultLocation "01111" "Study" (PerInvestigator 2) (Just Circle) []

hallway :: ArkhamLocationInternal
hallway = defaultLocation
  "01112"
  "Hallway"
  (Static 0)
  (Just Square)
  [Triangle, Plus, Diamond]

cellar :: ArkhamLocationInternal
cellar =
  (defaultLocation "01114" "Cellar" (PerInvestigator 2) (Just Plus) [Square])
    { aliOnEnter = \g i -> pure (g, i & healthDamage +~ 1)
    }

attic :: ArkhamLocationInternal
attic =
  (defaultLocation "01113" "Attic" (PerInvestigator 2) (Just Triangle) [Square])
    { aliOnEnter = \g i -> pure (g, i & sanityDamage +~ 1)
    }

parlor :: ArkhamLocationInternal
parlor = (defaultLocation "01115" "Parloc" (Static 0) (Just Diamond) [Square])
  { aliCanEnter = const False
  }
