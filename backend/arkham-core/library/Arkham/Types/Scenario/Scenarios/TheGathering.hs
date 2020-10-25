{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.TheGathering where

import Arkham.Import

import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait

newtype TheGathering = TheGathering Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theGathering :: Difficulty -> TheGathering
theGathering = TheGathering . baseAttrs
  "01104"
  "The Gathering"
  ["01105", "01106", "01107"]
  ["01108", "01109", "01110"]

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount (InvestigatorLocation, [Trait]) env, HasQueue env) => HasTokenValue env TheGathering where
  getTokenValue (TheGathering attrs) iid token = do
    case drawnTokenFace token of
      Token.Skull -> do
        ghoulCount <- asks $ unEnemyCount . getCount
          (InvestigatorLocation iid, [Ghoul])
        pure $ TokenValue
          token
          (NegativeModifier $ if isEasyStandard attrs then ghoulCount else 2)
      Token.Cultist -> pure $ TokenValue
        token
        (if isEasyStandard attrs then NegativeModifier 1 else NoModifier)
      Token.Tablet -> do
        pure $ TokenValue
          token
          (NegativeModifier $ if isEasyStandard attrs then 2 else 4)
      _other -> getTokenValue attrs iid token

instance (ScenarioRunner env) => RunMessage env TheGathering where
  runMessage msg s@(TheGathering attrs@Attrs {..}) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeck
        [ EncounterSet.TheGathering
        , EncounterSet.Rats
        , EncounterSet.Ghouls
        , EncounterSet.StrikingFear
        , EncounterSet.AncientEvils
        , EncounterSet.ChillingCold
        ]
      pushMessages
        [ SetEncounterDeck encounterDeck
        , AddAgenda "01105"
        , AddAct "01108"
        , PlaceLocation "01111"
        , RevealLocation "01111"
        , MoveAllTo "01111"
        , AskMap
        . mapFromList
        $ [ ( iid
            , ChooseOne
              [ Run
                  [ Continue "Continue"
                  , FlavorText
                    (Just "Part I: The Gathering")
                    [ "You and your partners have been investigating strange events taking place\
                      \ in your home city of Arkham, Massachusetts. Over the past few weeks,\
                      \ several townspeople have mysteriously gone missing. Recently, their\
                      \ corpses turned up in the woods, savaged and half - eaten. The police and\
                      \ newspapers have stated that wild animals are responsible, but you believe\
                      \ there is something else going on. You are gathered together at the lead\
                      \ investigator’s home to discuss these bizarre events."
                    ]
                  ]
              ]
            )
          | iid <- investigatorIds
          ]
        ]
      TheGathering <$> runMessage msg attrs
    ResolveToken token iid -> case drawnTokenFace token of
      Token.Skull -> pure s
      Token.Cultist ->
        s <$ when (isHardExpert attrs) (unshiftMessage $ DrawAnotherToken iid)
      Token.Tablet -> do
        ghoulCount <- asks $ unEnemyCount . getCount
          (InvestigatorLocation iid, [Ghoul])
        s <$ when
          (ghoulCount > 0)
          (unshiftMessage $ InvestigatorAssignDamage
            iid
            (DrawnTokenSource token)
            1
            (if isEasyStandard attrs then 0 else 1)
          )
      _other -> TheGathering <$> runMessage msg attrs
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ -> do
      case drawnTokenFace token of
        Token.Skull | isHardExpert attrs ->
          unshiftMessage $ FindAndDrawEncounterCard iid (EnemyType, Just Ghoul)
        Token.Cultist -> unshiftMessage $ InvestigatorAssignDamage
          iid
          (DrawnTokenSource token)
          0
          (if isEasyStandard attrs then 1 else 2)
        _ -> pure ()
      pure s
    NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You barely manage to escape\
                  \ your house with your lives. The woman from your parlor\
                  \ follows you out the front door, slamming it behind her. “You\
                  \ fools! See what you have done?” She pushes a chair in front of\
                  \ the door, lodging it beneath the doorknob. “We must get out\
                  \ of here. Come with me, and I will tell you what I know. We\
                  \ are the only ones who can stop the threat that lurks beneath\
                  \ from being unleashed throughout the city.” You’re in no state\
                  \ to argue. Nodding, you follow the woman as she runs from\
                  \ your front porch out into the rainy street, toward Rivertown."
                ]
              , Record YourHouseIsStillStanding
              , Record GhoulPriestIsStillAlive
              , chooseOne
                leadInvestigatorId
                [ Label
                  "Add Lita Chantler to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId "01117"]
                , Label "Do not add Lita Chantler to your deck" []
                ]
              ]
            <> [ GainXP iid (xp + 2) | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    Resolution 1 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You nod and allow the red-haired woman to\
                  \ set the walls and floor of your house ablaze. The fire spreads\
                  \ quickly, and you run out the front door to avoid being caught\
                  \ in the inferno. From the sidewalk, you watch as everything\
                  \ you own is consumed by the flames. “Come with me,” the\
                  \ woman says. “You must be told of the threat that lurks below.\
                  \ Alone, we are surely doomed…but together, we can stop it.”"
                ]
              , Record YourHouseHasBurnedToTheGround
              , chooseOne
                leadInvestigatorId
                [ Label
                  "Add Lita Chantler to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId "01117"]
                , Label "Do not add Lita Chantler to your deck" []
                ]
              , SufferTrauma leadInvestigatorId 0 1
              ]
            <> [ GainXP iid (xp + 2) | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    Resolution 2 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You refuse to follow the overzealous woman’s\
                  \ order and kick her out of your home for fear that she will set\
                  \ it ablaze without your permission. “Fools! You are making\
                  \ a grave mistake!” she warns. “You do not understand the\
                  \ threat that lurks below…the grave danger we are all in!”\
                  \ Still shaken by the night’s events, you decide to hear the\
                  \ woman out. Perhaps she can shed some light on these bizarre\
                  \ events…but she doesn’t seem to trust you very much."
                ]
              , Record YourHouseIsStillStanding
              , GainXP leadInvestigatorId 1
              ]
            <> [ GainXP iid (xp + 2) | iid <- investigatorIds ]
            <> [EndOfGame]
          ]
        )
    Resolution 3 -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
              [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You run to the hallway to try to find a way to\
                  \ escape the house, but the burning-hot barrier still blocks your\
                  \ path. Trapped, the horde of feral creatures that have invaded\
                  \ your home close in, and you have nowhere to run."
                ]
              , Record LitaWasForcedToFindOthersToHelpHerCause
              , Record YourHouseIsStillStanding
              , Record GhoulPriestIsStillAlive
              , chooseOne
                leadInvestigatorId
                [ Label
                  "Add Lita Chantler to your deck"
                  [AddCampaignCardToDeck leadInvestigatorId "01117"]
                , Label "Do not add Lita Chantler to your deck" []
                ]
              , EndOfGame
              ]
          ]
        )
    _ -> TheGathering <$> runMessage msg attrs
