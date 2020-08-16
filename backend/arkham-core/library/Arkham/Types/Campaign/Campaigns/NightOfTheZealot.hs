{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Campaigns.NightOfTheZealot where

import Arkham.Json
import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignId
import Arkham.Types.CampaignStep
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Message
import qualified Arkham.Types.Token as Token
import Arkham.Types.TokenPool
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype NightOfTheZealot = NightOfTheZealot Attrs
  deriving newtype (Show, ToJSON, FromJSON)

nightOfTheZealot :: Difficulty -> NightOfTheZealot
nightOfTheZealot difficulty =
  NightOfTheZealot
    $ (baseAttrs (CampaignId "01") "Night of the Zealot" difficulty tokenPool)
        { campaignSteps = fromList
          [ PrologueStep
          , ScenarioStep "01104"
          , ScenarioStep "01120"
          , ScenarioStep "01142"
          ]
        }
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

instance (CampaignRunner env) => RunMessage env NightOfTheZealot where
  runMessage msg c@(NightOfTheZealot attrs@Attrs {..}) = case msg of
    StartCampaign -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      c <$ unshiftMessages
        [ AskMap
        . HashMap.fromList
        $ [ ( iid
            , ChooseOne
              [ Run
                  [ Continue "Continue"
                  , FlavorText (Just "The Ghouls Hunger...") ""
                  ]
              ]
            )
          | iid <- investigatorIds
          ]
        , StepCampaign
        ]
    StepCampaign -> if length campaignSteps > campaignStep - 1
      then c <$ unshiftMessage GameOver
      else pure $ NightOfTheZealot $ attrs & step +~ 1
    _ -> NightOfTheZealot <$> runMessage msg attrs
