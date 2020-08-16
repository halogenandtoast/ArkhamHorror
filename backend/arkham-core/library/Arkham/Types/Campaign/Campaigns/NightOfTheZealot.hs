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
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Vector ((!?))
import Lens.Micro

newtype NightOfTheZealot = NightOfTheZealot Attrs
  deriving newtype (Show, ToJSON, FromJSON)

nightOfTheZealot :: Difficulty -> NightOfTheZealot
nightOfTheZealot difficulty =
  NightOfTheZealot
    $ (baseAttrs
        (CampaignId "01")
        "Night of the Zealot"
        difficulty
        chaosBagContents
      )
        { campaignSteps = fromList
          [ PrologueStep
          , ScenarioStep "01104"
          , ScenarioStep "01120"
          , ScenarioStep "01142"
          ]
        }
 where
  chaosBagContents = case difficulty of
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
    StartCampaign ->
      c <$ unshiftMessage (CampaignStep $ campaignSteps !? campaignStep)
    CampaignStep Nothing -> c <$ unshiftMessage GameOver -- TODO: move to generic
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      c <$ unshiftMessages
        [ AskMap
        . HashMap.fromList
        $ [ ( iid
            , ChooseOne
              [ Run
                  [ Continue "Continue"
                  , FlavorText
                    (Just "The Ghouls Hunger...")
                    [ "Friday, September 18, 1925. Arkham, Massachusetts. It is\
                      \ the end of a long and abnormally hot summer. The first hints\
                      \ of autumn beckon, but a heavy heat persists, relentless. A\
                      \ silent, unspoken anger grips the town. Tempers are short, and\
                      \ in the last week alone there have been numerous reports of\
                      \ townspeople coming to heated, violent blows with one another\
                      \ over simple misunderstandings."
                    , "And now, a call from James Hankerson. He claims to have\
                      \ found a dismembered body in his barn."
                    , "Blaming the weather would be too easy. There is something\
                      \ wrong with this town, and not a whole lot this old soothsayer\
                      \ can do to stop the slide. My auguries indicate a small group of\
                      \ investigators will soon take note of these strange happenings\
                      \ and set forth to make things right. I’ll be watching their\
                      \ progress…but I won’t be holding my breath."
                    ]
                  ]
              ]
            )
          | iid <- investigatorIds
          ]
        , NextCampaignStep
        ]
    CampaignStep (Just (ScenarioStep sid)) -> do
      c <$ unshiftMessages [ResetGame, StartScenario sid]
    NextCampaignStep -> do
      clearQueue
      unshiftMessage (CampaignStep $ campaignSteps !? (campaignStep + 1))
      pure $ NightOfTheZealot $ attrs & step +~ 1
    _ -> NightOfTheZealot <$> runMessage msg attrs
