module Arkham.Types.Scenario.Scenarios.TheGathering where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet qualified as EncounterSet
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (RevealLocation)
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
import Arkham.Types.Trait qualified as Trait

newtype TheGathering = TheGathering ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasRecord)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theGathering :: Difficulty -> TheGathering
theGathering difficulty =
  TheGathering
    $ baseAttrs
        "01104"
        "The Gathering"
        [ Agendas.whatsGoingOn
        , Agendas.riseOfTheGhouls
        , Agendas.theyreGettingOut
        ]
        [Acts.trapped, Acts.theBarrier, Acts.whatHaveYouDone]
        difficulty
    & locationLayoutL
    ?~ [ "   .   attic   .     "
       , " study hallway parlor"
       , "   .   cellar  .     "
       ]

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

instance ScenarioRunner env => RunMessage env TheGathering where
  runMessage msg s@(TheGathering attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.ghoulPriest]
        [ EncounterSet.TheGathering
        , EncounterSet.Rats
        , EncounterSet.Ghouls
        , EncounterSet.StrikingFear
        , EncounterSet.AncientEvils
        , EncounterSet.ChillingCold
        ]
      study <- genCard Locations.study
      let studyId = toLocationId study

      pushAllEnd
        [ SetEncounterDeck encounterDeck
        , AddAgenda "01105"
        , AddAct "01108"
        , PlaceLocation study
        , RevealLocation Nothing studyId
        , MoveAllTo (toSource attrs) studyId
        , story investigatorIds theGatheringIntro
        ]

      setAsideCards <- traverse
        genCard
        [ Enemies.ghoulPriest
        , Assets.litaChantler
        , Locations.hallway
        , Locations.attic
        , Locations.cellar
        , Locations.parlor
        ]

      TheGathering <$> runMessage msg (attrs & setAsideCardsL .~ setAsideCards)
    ResolveToken _ Cultist iid ->
      s <$ when (isHardExpert attrs) (push $ DrawAnotherToken iid)
    ResolveToken _ Tablet iid -> do
      ghoulCount <- unEnemyCount
        <$> getCount (InvestigatorLocation iid, [Trait.Ghoul])
      s <$ when
        (ghoulCount > 0)
        (push $ InvestigatorAssignDamage
          iid
          (TokenEffectSource Tablet)
          DamageAny
          1
          (if isEasyStandard attrs then 0 else 1)
        )
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        Skull | isHardExpert attrs -> push $ FindAndDrawEncounterCard
          iid
          (CardWithType EnemyType <> CardWithTrait Trait.Ghoul)
        Cultist -> push $ InvestigatorAssignDamage
          iid
          (TokenSource token)
          DamageAny
          0
          (if isEasyStandard attrs then 1 else 2)
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
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
             [AddCampaignCardToDeck leadInvestigatorId Assets.litaChantler]
           , Label "Do not add Lita Chantler to your deck" []
           ]
         ]
        <> [ GainXP iid (n + 2) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 1")
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
             [AddCampaignCardToDeck leadInvestigatorId Assets.litaChantler]
           , Label "Do not add Lita Chantler to your deck" []
           ]
         , SufferTrauma leadInvestigatorId 0 1
         ]
        <> [ GainXP iid (n + 2) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      xp <- getXp
      s <$ pushAll
        ([ chooseOne
           leadInvestigatorId
           [ Run
               [ Continue "Continue"
               , FlavorText
                 (Just "Resolution 2")
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
        <> [ GainXP iid (n + 2) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
    ScenarioResolution (Resolution 3) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ pushAll
        [ chooseOne
          leadInvestigatorId
          [ Run
              [ Continue "Continue"
              , FlavorText
                (Just "Resolution 3")
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
            [AddCampaignCardToDeck leadInvestigatorId Assets.litaChantler]
          , Label "Do not add Lita Chantler to your deck" []
          ]
        , EndOfGame Nothing
        ]
    _ -> TheGathering <$> runMessage msg attrs
