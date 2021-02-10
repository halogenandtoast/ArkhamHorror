module Arkham.Types.Scenario.Scenarios.TheDevourerBelow where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window
 hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token
import Arkham.Types.Trait hiding (Cultist)
import Data.List.NonEmpty (NonEmpty(..))

newtype TheDevourerBelow = TheDevourerBelow ScenarioAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDevourerBelow :: Difficulty -> TheDevourerBelow
theDevourerBelow difficulty =
  TheDevourerBelow $ (baseAttrs "01142" "The Devourer Below" [] [] difficulty)
    { scenarioLocationLayout = Just
      [ "woods1     .     woods2"
      , "woods1 mainPath woods2"
      , "woods3 mainPath woods4"
      , "woods3 ritualSite woods4"
      , "   .   ritualSite   .  "
      ]
    }

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount env [Trait]) => HasTokenValue env TheDevourerBelow where
  getTokenValue (TheDevourerBelow attrs) iid = \case
    Skull -> do
      monsterCount <- unEnemyCount <$> getCount [Monster]
      pure $ toTokenValue attrs Skull monsterCount 3
    Cultist -> pure $ toTokenValue attrs Cultist 2 4
    Tablet -> pure $ toTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toTokenValue attrs ElderThing 5 7
    otherFace -> getTokenValue attrs iid otherFace

instance (ScenarioRunner env) => RunMessage env TheDevourerBelow where
  runMessage msg s@(TheDevourerBelow attrs@ScenarioAttrs {..}) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      pastMidnight <- getHasRecord ItIsPastMidnight
      ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
      cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
      ghoulPriestCard <- lookupEncounterCard "01116" <$> getRandom
      let
        arkhamWoods = ["01150", "01151", "01152", "01153", "01154", "01155"]
        woodsLabels = ["woods1", "woods2", "woods3", "woods4"]
        ghoulPriestMessages =
          [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
        pastMidnightMessages =
          if pastMidnight then [AllRandomDiscard, AllRandomDiscard] else []
        cultistsWhoGotAwayMessages =
          replicate ((length cultistsWhoGotAway + 1) `div` 2) PlaceDoomOnAgenda
      woodsLocations <- take 4 <$> shuffleM arkhamWoods
      randomSet <-
        sample
        $ EncounterSet.AgentsOfYogSothoth
        :| [ EncounterSet.AgentsOfShubNiggurath
           , EncounterSet.AgentsOfCthulhu
           , EncounterSet.AgentsOfHastur
           ]
      encounterDeck <- buildEncounterDeck
        [ EncounterSet.TheDevourerBelow
        , EncounterSet.AncientEvils
        , EncounterSet.StrikingFear
        , EncounterSet.Ghouls
        , EncounterSet.DarkCult
        , randomSet
        ]
      pushMessages
        $ [ AskMap
            (mapFromList
              [ ( iid
                , ChooseOne
                  [ Run
                      [ Continue "Continue"
                      , FlavorText
                        (Just "Part III: The Devourer Below")
                        [ "After a frantic nighttime search throughout Arkham, you have tracker\
                              \ down and questioned several members of the cult. Your findings are\
                              \ disturbing: they claim to worship a being known as Umôrdhoth, a\
                              \ monstrous entity from another realm."
                        , "You are able to confirm much of Lita’s story: the cult is agitated over\
                              \ the destruction of a ghoul lair. However, a surprising detail also turns\
                              \ up: the one who invaded the lair and set this night’s events in motion\
                              \ was none other than Lita Chantler herself! You are not sure why this\
                              \ important detail was omitted from Lita’s story—did she tell you only\
                              \ as much as was necessary to draw you into her conflict? But in another\
                              \ light, she seems to be fighting to protect the city of Arkham from a\
                              \ terrible menace."
                        , "The final piece of the puzzle was found written in a journal possessed by\
                              \ one of the cultists. It describes a dark ritual to be performed deep within\
                              \ the woods south of Arkham, this very night. According to the journal,\
                              \ the ritual’s completion will open a gate and bring forth the cult’s dark\
                              \ master into this world. “If the cult is not stopped,” Lita warns, “there is\
                              \ a possibility that Umôrdhoth’s vengeance will consume all in its path.”\
                              \ Frightened but determined to stop the ritual, you head into the woods…"
                        ]
                      ]
                  ]
                )
              | iid <- investigatorIds
              ]
            )
          , SetEncounterDeck encounterDeck
          , AddToken ElderThing
          , AddAgenda "01143"
          , AddAct "01146"
          , PlaceLocation "01149"
          ]
        <> [ PlaceLocation location | location <- woodsLocations ]
        <> [ SetLocationLabel location label
           | (location, label) <- zip woodsLocations woodsLabels
           ]
        <> [RevealLocation Nothing "01149", MoveAllTo "01149"]
        <> ghoulPriestMessages
        <> cultistsWhoGotAwayMessages
        <> pastMidnightMessages
      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocation))
          (["01149", "01156"] <> woodsLocations)
      TheDevourerBelow <$> runMessage msg (attrs & locationsL .~ locations')
    ResolveToken _ Cultist iid -> do
      let doom = if isEasyStandard attrs then 1 else 2
      closestEnemyIds <- map unClosestEnemyId <$> getSetList iid
      case closestEnemyIds of
        [] -> pure ()
        [x] -> unshiftMessage (PlaceDoom (EnemyTarget x) doom)
        xs -> unshiftMessage
          (chooseOne iid [ PlaceDoom (EnemyTarget x) doom | x <- xs ])
      pure s
    ResolveToken _ Tablet iid -> do
      let horror = if isEasyStandard attrs then 0 else 1
      monsterCount <- unEnemyCount
        <$> getCount (InvestigatorLocation iid, [Monster])
      s <$ when
        (monsterCount > 0)
        (unshiftMessage $ InvestigatorAssignDamage
          iid
          (TokenEffectSource Tablet)
          DamageAny
          1
          horror
        )
    ResolveToken _ ElderThing iid -> do
      ancientOneCount <- unEnemyCount <$> getCount [AncientOne]
      s <$ when (ancientOneCount > 0) (unshiftMessage $ DrawAnotherToken iid)
    FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _
      | isHardExpert attrs && drawnTokenFace token == Skull -> s
      <$ unshiftMessage
           (FindAndDrawEncounterCard
             iid
             (EncounterCardMatchByType (EnemyType, Just Monster))
           )
    ScenarioResolution NoResolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "Too frightened to face her fate, Lita flees\
                  \ into the night. She realizes that she has failed and Umôrdhoth’s\
                  \ vengeance will pursue her wherever she goes. The creature’s\
                  \ tendrils spread throughout the city of Arkham, searching for\
                  \ her. It lurks in the darkness of every corner, tugging at the seams\
                  \ of reality. But Lita is nowhere to be found, so the creature dwells\
                  \ in the shadows to this day, searching…killing"
                ]
              , Record ArkhamSuccumbedToUmordhothsTerribleVengeance
              ]
            <> [EndOfGame]
          ]
        )
    ScenarioResolution (Resolution 1) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "You have managed to prevent the cult from\
                  \ summoning its master. Although you’re unsure what would\
                  \ have happened had the cult succeeded, you’re relieved that—at\
                  \ least for the time being—Arkham is safe. You capture as many\
                  \ cultists as you can find, but very few townspeople believe your\
                  \ tale. Perhaps it was all in your head, after all."
                ]
              , Record TheRitualToSummonUmordhothWasBroken
              ]
            <> [EndOfGame]
          ]
        )
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "Through force of arms and strength of will,\
                  \ you are somehow able to harm Umôrdhoth enough to send it\
                  \ reeling back to the dimension from which it emerged. Warmth\
                  \ and light return to the woods as the void-like mass is sucked in\
                  \ upon itself, vanishing in an instant. You aren’t sure if a being\
                  \ such as this can be killed, but for the time being it seems to have\
                  \ retreated. As their master vanishes, the ghouls nearby climb\
                  \ into the open pit below, fleeing with terrible cries and shrieks.\
                  \ You have stopped an evil plot, but the fight has taken its toll on\
                  \ your body and mind. Worse, you can’t help but feel insignificant\
                  \ in the face of the world’s mysteries. What other terrors exist in\
                  \ the deep, dark corners of reality?"
                ]
              , Record TheInvestigatorsRepelledUmordoth
              ]
            <> [EndOfGame]
          ]
        )
    ScenarioResolution (Resolution 3) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Run
            $ [ Continue "Continue"
              , FlavorText
                Nothing
                [ "In the face of this horror, you don’t believe there\
                  \ is anything you can do to stop it. You have but one hope if you\
                  \ are to survive. You turn on Lita and throw her at the terrible\
                  \ monstrosity, watching in dread as its swirling void-like mass\
                  \ consumes her. She cries out in torment as the life is sucked from\
                  \ her body. “Umôrdhoth…Umôrdhoth…” the cultists chant.\
                  \ Lita Chantler vanishes without a trace. For a moment, you\
                  \ fear that the creature will now turn on you, but you hear one of\
                  \ the cultists say, “Umôrdhoth is a just god who claims only the\
                  \ guilty and the dead. Go, and you shall be spared.” The swirling\
                  \ mass vanishes, and warmth and light return to the woods. The\
                  \ cultists slink away, leaving you alive. Lita’s last moments are\
                  \ forever etched upon your memory."
                ]
              , Record TheInvestigatorsSacrificedLitaChantlerToUmordhoth
              ]
            <> [EndOfGame]
          ]
        )
    _ -> TheDevourerBelow <$> runMessage msg attrs
