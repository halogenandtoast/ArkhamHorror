module Arkham.Internal.Event where

import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Skill
import ClassyPrelude

data ArkhamEventInternal = ArkhamEventInternal
  { eventName :: Text
  , eventCardCode :: ArkhamCardCode
  , eventCost :: Int
  , eventTestIcons :: [ArkhamSkillType]
  , eventAfterPlay :: forall m. MonadIO => ArkhamGameState -> ArkhamPlayer -> m ArkhamGameState
  }

event :: Text -> ArkhamCardCode -> Int -> ArkhamEventInternal
event name code' cost = undefined

emergencyCache :: ArkhamEventInternal
emergencyCache = (event "Emergency Cache" 0)
  { eventAfterPlay = \g _ -> pure $ g & activePlayer . resources +~ 3
  }

dodge :: ArkhamEventInternal
dodge = (event 1) { eventTestIcons = [willpower, agility] }

dynamiteBlast :: ArkhamEventInternal
dynamiteBlast = (event 5) { eventTestIcons = [willpower] }

evidence :: ArkhamEventInternal
evidence = (event 1) { eventTestIcons = replicate 2 intellect }

-- brittany-disable-next-binding
workingAHunch :: ArkhamEventInternal
workingAHunch = fast $ (event 2)
  { eventTestIcons = replicate 2 intellect
  , eventAfterPlay = \g ->
    let location = locationFor (g ^. activePlayer) g
    in
      if alClues location > 0
        then g & locations . at (alCardCode location) . _Just . clues -~ 1
               & activePlayer . clues +~ 1
        else g
  }
