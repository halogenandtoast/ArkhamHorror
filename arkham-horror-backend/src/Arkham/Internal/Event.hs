module Arkham.Internal.Event
  ( ArkhamEventInternal(..)
  , allEvents
  )
where

import Arkham.Internal.Location
import Arkham.Types
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Location
import Arkham.Types.Player
import Arkham.Types.Skill
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Lens.Micro.Platform ()

data ArkhamEventInternal = ArkhamEventInternal
  { eventName :: Text
  , eventCode :: ArkhamCardCode
  , eventCost :: Int
  , eventTraits :: HashSet ArkhamTrait
  , eventImage :: Text
  , eventTestIcons :: [ArkhamSkillType]
  , eventAfterPlay :: forall m. MonadIO m => ArkhamGameState -> ArkhamPlayer -> m ArkhamGameState
  , eventIsFast :: Bool
  , eventActionCost :: forall m. MonadIO m => ArkhamGameState -> ArkhamPlayer -> m Int
  }

allEvents :: HashMap ArkhamCardCode ArkhamEventInternal
allEvents = HashMap.fromList $ map
  (\e -> (eventCode e, e))
  [emergencyCache, dodge, dynamiteBlast, evidence, workingAHunch]

event :: Text -> ArkhamCardCode -> Int -> ArkhamEventInternal
event name code cost = ArkhamEventInternal
  { eventName = name
  , eventCode = code
  , eventCost = cost
  , eventImage =
    "https://arkhamdb.com/bundles/cards/" <> unArkhamCardCode code <> ".png"
  , eventTestIcons = []
  , eventAfterPlay = \g _ -> pure g
  , eventIsFast = False
  , eventActionCost = const (const (pure 1))
  , eventTraits = mempty
  }

fast :: ArkhamEventInternal -> ArkhamEventInternal
fast c = c { eventIsFast = True }

emergencyCache :: ArkhamEventInternal
emergencyCache = (event "Emergency Cache" "01088" 0)
  { eventAfterPlay = \g _ -> pure $ g & activePlayer . resources +~ 3
  , eventImage = "https://arkhamdb.com/bundles/cards/01088.jpg"
  }

dodge :: ArkhamEventInternal
dodge = (event "Dodge" "01023" 1)
  { eventTestIcons = [ArkhamSkillWillpower, ArkhamSkillAgility]
  }

dynamiteBlast :: ArkhamEventInternal
dynamiteBlast =
  (event "Dynamite Blast" "01024" 5) { eventTestIcons = [ArkhamSkillWillpower] }

evidence :: ArkhamEventInternal
evidence = (event "Evidence!" "01022" 1)
  { eventTestIcons = replicate 2 ArkhamSkillIntellect
  }

-- brittany-disable-next-binding
workingAHunch :: ArkhamEventInternal
workingAHunch = fast $ (event "Working a Hunch" "01037" 2)
  { eventTestIcons = replicate 2 ArkhamSkillIntellect
  , eventAfterPlay = \g _ ->
    let location = locationFor (g ^. activePlayer) g
    in
      if alClues location > 0
        then pure $ g & locations . at (alCardCode location) . _Just . clues -~ 1
               & activePlayer . clues +~ 1
        else pure g
  }
