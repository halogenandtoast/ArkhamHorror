{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act
  ( Act(..)
  , lookupAct
  , canAdvance
  )
where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

lookupAct :: ActId -> Act
lookupAct = fromJustNote "Unknown act" . flip HashMap.lookup allActs

allActs :: HashMap ActId Act
allActs = HashMap.fromList
  $ map (\a -> (actId $ actAttrs a, a)) [trapped, theBarrier, whatHaveYouDone]

data Attrs = Attrs
  { actCanAdvance :: Bool
  , actId         :: ActId
  , actName       :: Text
  , actSequence   :: Text
  , actAbilities :: [Ability]
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "act"
  toEncoding = genericToEncoding $ aesonOptions $ Just "act"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "act"

canAdvance :: Lens' Attrs Bool
canAdvance = lens actCanAdvance $ \m x -> m { actCanAdvance = x }

instance HasAbilities Act where
  getAbilities = actAbilities . actAttrs

instance IsAdvanceable Act where
  isAdvanceable = actCanAdvance . actAttrs

baseAttrs :: ActId -> Text -> Text -> Attrs
baseAttrs aid name seq' = Attrs
  { actCanAdvance = False
  , actId = aid
  , actName = name
  , actSequence = seq'
  , actAbilities = mempty
  }

data Act
  = Trapped TrappedI
  | TheBarrier TheBarrierI
  | WhatHaveYouDone WhatHaveYouDoneI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

actAttrs :: Act -> Attrs
actAttrs = \case
  Trapped attrs -> coerce attrs
  TheBarrier attrs -> coerce attrs
  WhatHaveYouDone attrs -> coerce attrs

newtype TrappedI = TrappedI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trapped :: Act
trapped = Trapped . TrappedI $ baseAttrs "01108" "Trapped" "Act 1a"

newtype TheBarrierI = TheBarrierI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theBarrier :: Act
theBarrier =
  TheBarrier . TheBarrierI $ baseAttrs "01109" "The Barrier" "Act 2a"

newtype WhatHaveYouDoneI = WhatHaveYouDoneI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatHaveYouDone :: Act
whatHaveYouDone = WhatHaveYouDone . WhatHaveYouDoneI $ baseAttrs
  "01110"
  "What Have You Done?"
  "Act 3a"

type ActRunner env
  = ( HasQueue env
    , HasCount ClueCount AllInvestigators env
    , HasCount ClueCount InvestigatorId env
    , HasSet EnemyId LocationId env
    , HasSet InvestigatorId LocationId env
    , HasCount PlayerCount () env
    , HasSet InvestigatorId () env
    )

instance (ActRunner env) => RunMessage env Act where
  runMessage msg = \case
    Trapped x -> Trapped <$> runMessage msg x
    TheBarrier x -> TheBarrier <$> runMessage msg x
    WhatHaveYouDone x -> WhatHaveYouDone <$> runMessage msg x

instance (ActRunner env) => RunMessage env TrappedI where
  runMessage msg a@(TrappedI attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId -> do
      enemyIds <- HashSet.toList <$> asks (getSet (LocationId "01111"))
      playerCount <- unPlayerCount <$> asks (getCount ())
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      a <$ unshiftMessages
        ([ SpendClues (fromGameValue (PerPlayer 2) playerCount) investigatorIds
         , PlaceLocation "01112"
         , PlaceLocation "01114"
         , PlaceLocation "01113"
         , PlaceLocation "01115"
         ]
        <> map RemoveEnemy enemyIds
        <> [ RevealLocation "01112"
           , MoveAllTo "01112"
           , RemoveLocation "01111"
           , NextAct aid "01109"
           ]
        )
    PrePlayerWindow -> do
      clueCount <- unClueCount <$> asks (getCount AllInvestigators)
      playerCount <- unPlayerCount <$> asks (getCount ())
      pure
        $ TrappedI
        $ attrs
        & canAdvance
        .~ (clueCount >= fromGameValue (PerPlayer 2) playerCount)
    _ -> TrappedI <$> runMessage msg attrs

instance (ActRunner env) => RunMessage env TheBarrierI where
  runMessage msg a@(TheBarrierI attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId -> a <$ unshiftMessages
      [ RevealLocation "01115"
      , CreateStoryAssetAt "01117" "01115"
      , CreateEnemyAt "01116" "01112"
      , NextAct aid "01110"
      ]
    EndRoundWindow -> do
      investigatorIds <- asks (getSet (LocationId "01112"))
      clueCount <- unClueCount . mconcat <$> traverse
        (asks . getCount @ClueCount)
        (HashSet.toList investigatorIds)
      playerCount <- unPlayerCount <$> asks (getCount ())
      let requiredClueCount = fromGameValue (PerPlayer 3) playerCount
      if clueCount >= requiredClueCount
        then a <$ unshiftMessage
          (Ask $ ChooseToDoAll
            [ SpendClues requiredClueCount (HashSet.toList investigatorIds)
            , AdvanceAct actId
            ]
          )
        else pure a
    _ -> TheBarrierI <$> runMessage msg attrs

instance (ActRunner env) => RunMessage env WhatHaveYouDoneI where
  runMessage msg a@(WhatHaveYouDoneI attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId ->
      a <$ unshiftMessage (Ask $ ChooseOne [Resolution 1, Resolution 2])
    EnemyDefeated _ _ "01116" _ -> a <$ unshiftMessage (AdvanceAct actId)
    _ -> WhatHaveYouDoneI <$> runMessage msg attrs

instance (HasQueue env) => RunMessage env Attrs where
  runMessage _msg a@Attrs {..} = pure a
