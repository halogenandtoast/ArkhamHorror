module Arkham.Types.Scenario.Scenarios.TheGathering where

import Arkham.Prelude

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait (Trait)
import qualified Arkham.Types.Trait as Trait

newtype TheGathering = TheGathering ScenarioAttrs
  deriving stock Generic
  deriving anyclass HasRecord
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theGathering :: Difficulty -> TheGathering
theGathering difficulty = TheGathering $ base
  { scenarioLocationLayout = Just
    ["   .   attic   .     ", " study hallway parlor", "   .   cellar  .     "]
  }
 where
  base = baseAttrs
    "01104"
    "The Gathering"
    ["01105", "01106", "01107"]
    ["01108", "01109", "01110"]
    difficulty

theGatheringIntro :: Message
theGatheringIntro = FlavorText
  (Just "Part I: The Gathering")
  [ "You and your partners have been investigating strange events taking place\
    \ in your home city of Arkham, Massachusetts. Over the past few weeks,\
    \ several townspeople have mysteriously gone missing. Recently, their\
    \ corpses turned up in the woods, savaged and half - eaten. The police and\
    \ newspapers have stated that wild animals are responsible, but you believe\
    \ there is something else going on. You are gathered together at the lead\
    \ investigator’s home to discuss these bizarre events."
  ]

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount env (InvestigatorLocation, [Trait])) => HasTokenValue env TheGathering where
  getTokenValue (TheGathering attrs) iid = \case
    Skull -> do
      ghoulCount <- unEnemyCount
        <$> getCount (InvestigatorLocation iid, [Trait.Ghoul])
      pure $ toTokenValue attrs Skull ghoulCount 2
    Cultist -> pure $ TokenValue
      Cultist
      (if isEasyStandard attrs then NegativeModifier 1 else NoModifier)
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    otherFace -> getTokenValue attrs iid otherFace

instance (HasId (Maybe LocationId) env LocationMatcher, ScenarioRunner env) => RunMessage env TheGathering where
  runMessage msg s@(TheGathering attrs) = case msg of
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
      studyId <- getRandom
      pushMessages
        [ SetEncounterDeck encounterDeck
        , AddAgenda "01105"
        , AddAct "01108"
        , PlaceLocation "01111" studyId
        , RevealLocation Nothing studyId
        , MoveAllTo studyId
        , AskMap
        . mapFromList
        $ [ (iid, ChooseOne [Run [Continue "Continue", theGatheringIntro]])
          | iid <- investigatorIds
          ]
        ]
      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocationStub))
          ["01111", "01112", "01113", "01114", "01115"]
      TheGathering <$> runMessage msg (attrs & locationsL .~ locations')
    ResolveToken _ Cultist iid ->
      s <$ when (isHardExpert attrs) (unshiftMessage $ DrawAnotherToken iid)
    ResolveToken _ Tablet iid -> do
      ghoulCount <- unEnemyCount
        <$> getCount (InvestigatorLocation iid, [Trait.Ghoul])
      s <$ when
        (ghoulCount > 0)
        (unshiftMessage $ InvestigatorAssignDamage
          iid
          (TokenEffectSource Tablet)
          DamageAny
          1
          (if isEasyStandard attrs then 0 else 1)
        )
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _ -> do
      case drawnTokenFace token of
        Skull | isHardExpert attrs -> unshiftMessage $ FindAndDrawEncounterCard
          iid
          (CardMatchByType (EnemyType, singleton Trait.Ghoul))
        Cultist -> unshiftMessage $ InvestigatorAssignDamage
          iid
          (DrawnTokenSource token)
          DamageAny
          0
          (if isEasyStandard attrs then 1 else 2)
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
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
               ]
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
        )
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
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
               ]
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
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
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
               ]
           ]
         , Record YourHouseIsStillStanding
         , GainXP leadInvestigatorId 1
         ]
        <> [ GainXP iid (xp + 2) | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 3) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessages
        [ chooseOne
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
              ]
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
    _ -> TheGathering <$> runMessage msg attrs
