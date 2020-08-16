{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Campaigns.TheDunwichLegacy where

import Arkham.Json
import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignId
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.Token as Token
import Arkham.Types.TokenPool
import ClassyPrelude

newtype TheDunwichLegacy = TheDunwichLegacy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theDunwichLegacy :: Difficulty -> TheDunwichLegacy
theDunwichLegacy difficulty = TheDunwichLegacy
  $ baseAttrs (CampaignId "02") "The Dunwich Legacy" difficulty tokenPool
 where
  tokenPool = TokenPool $ case difficulty of
    Easy ->
      [ Token.PlusOne
      , Token.PlusOne
      , Token.Zero
      , Token.Zero
      , Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]
    Standard ->
      [ Token.PlusOne
      , Token.Zero
      , Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.MinusThree
      , Token.MinusFour
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]
    Hard ->
      [ Token.Zero
      , Token.Zero
      , Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.MinusThree
      , Token.MinusThree
      , Token.MinusFour
      , Token.MinusFive
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]
    Expert ->
      [ Token.Zero
      , Token.MinusOne
      , Token.MinusOne
      , Token.MinusTwo
      , Token.MinusTwo
      , Token.MinusThree
      , Token.MinusThree
      , Token.MinusFour
      , Token.MinusFour
      , Token.MinusFive
      , Token.MinusSix
      , Token.MinusEight
      , Token.Skull
      , Token.Skull
      , Token.Cultist
      , Token.Tablet
      , Token.AutoFail
      , Token.ElderSign
      ]

instance (CampaignRunner env) => RunMessage env TheDunwichLegacy where
  runMessage msg (TheDunwichLegacy attrs) =
    TheDunwichLegacy <$> runMessage msg attrs
