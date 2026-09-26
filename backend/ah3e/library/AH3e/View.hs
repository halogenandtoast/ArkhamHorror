{- | What a client needs to draw the table: the static catalog (scenarios,
expansions, investigators) and, for a game, the game itself plus the facts a
viewer can't cheaply derive on its own.
-}
module AH3e.View (catalogView, gameView) where

import AH3e.Content (ScenarioInfo (..), cardDef, investigatorDefs, scenarioCatalog, scenarioDefs)
import AH3e.Engine.Hooks (actionAllowance, effectiveMonsterHealth)
import AH3e.Engine.Query (unstableSpaces)
import AH3e.Game
import AH3e.Prelude hiding ((.=))
import AH3e.Types.Card
import Control.Monad.State.Strict (evalState)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map

catalogView :: [Pair]
catalogView =
  [ "scenarios"
      .= [ object
             [ "code" .= i.code
             , "name" .= i.name
             , "expansion" .= i.expansion
             , "playable" .= Map.member i.code scenarioDefs
             , "anomalySet" .= (Map.lookup i.code scenarioDefs >>= (.anomalySet))
             ]
         | i <- scenarioCatalog
         ]
  , "expansions" .= [CoreSet, DeadOfNight, UnderDarkWaves, SecretsOfTheOrder, RecursiveEchoes]
  , "investigatorNames" .= Map.map (.name) investigatorDefs
  , "investigatorDefs" .= investigatorDefs
  ]

gameView :: Game -> Value
gameView g =
  object
    [ "game" .= g
    , "cardNames" .= Map.map (\c -> maybe (tshow c) (.name) (cardDef c)) g.cards
    , "cardCodes" .= g.cards
    , "eventNeighborhoods"
        .= Map.mapMaybe
          ( \c -> case (.kind) <$> cardDef c of
              Just (EventCard e) -> Just e.neighborhood
              _ -> Nothing
          )
          g.cards
    , -- only derivable once a scenario is chosen
      "unstable" .= maybe [] (const (evalState unstableSpaces g)) g.scenario
    , -- 451.3: a massive monster stays in its space even while engaged
      "massive"
        .= [ cid
           | cid <- Map.keys g.monsters
           , Just (MonsterCard d) <- [(.kind) <$> (Map.lookup cid g.cards >>= cardDef)]
           , Massive `elem` d.keywords
           ]
    , -- a card may reduce a monster's health, so the board is told what it is now
      "monsterHealth"
        .= Map.fromList
          [ (cid, h)
          | cid <- Map.keys g.monsters
          , Just h <- [evalState (effectiveMonsterHealth cid) g]
          ]
    , -- how many actions each investigator may take on their turn
      "actionAllowance"
        .= Map.fromList [(iid, evalState (actionAllowance iid) g) | iid <- Map.keys g.investigators]
    ]
