module Arkham.Internal.Location
  ( allLocations
  , ArkhamLocationInternal(..)
  , toLocationInternal
  )
where

import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Arkham.Types.Location
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Safe (fromJustNote)

data ArkhamLocationInternal = ArkhamLocationInternal
  { aliCardCode :: ArkhamCardCode
  , aliOnReveal :: ArkhamGameState -> ArkhamLocation -> ArkhamLocation
  , aliOnEnter :: forall m. MonadIO m => ArkhamGameState -> ArkhamInvestigator -> m (ArkhamGameState, ArkhamInvestigator)
  , aliCanEnter :: ArkhamGameState -> Bool
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
  , aliCanEnter = const True
  , aliOnEnter = curry pure
  }

hallway :: ArkhamLocationInternal
hallway = ArkhamLocationInternal
  { aliCardCode = ArkhamCardCode "01112"
  , aliOnReveal = \_ l -> l { alStatus = Revealed }
  , aliCanEnter = const True
  , aliOnEnter = curry pure
  }

cellar :: ArkhamLocationInternal
cellar = ArkhamLocationInternal
  { aliCardCode = ArkhamCardCode "01114"
  , aliOnEnter = \g i -> pure (g, i & healthDamage +~ 1)
  , aliCanEnter = const True
  , aliOnReveal = \g l ->
    (l { alStatus = Revealed }) & clues .~ (2 * length (g ^. players))
  }

attic :: ArkhamLocationInternal
attic = ArkhamLocationInternal
  { aliCardCode = ArkhamCardCode "01113"
  , aliOnEnter = \g i -> pure (g, i & sanityDamage +~ 1)
  , aliCanEnter = const True
  , aliOnReveal = \g l ->
    (l { alStatus = Revealed }) & clues .~ (2 * length (g ^. players))
  }

parlor :: ArkhamLocationInternal
parlor = ArkhamLocationInternal
  { aliCardCode = ArkhamCardCode "01115"
  , aliOnEnter = curry pure
  , aliCanEnter = const False
  , aliOnReveal = flip const
  }
