module Arkham.Types.Scenario.Scenarios.ReturnToTheDevourerBelow where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheDevourerBelow
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait hiding (Cultist)

newtype ReturnToTheDevourerBelow = ReturnToTheDevourerBelow TheDevourerBelow
  deriving stock Generic
  deriving anyclass HasRecord
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheDevourerBelow :: Difficulty -> ReturnToTheDevourerBelow
returnToTheDevourerBelow difficulty =
  ReturnToTheDevourerBelow
    . TheDevourerBelow
    $ (baseAttrs "01142" "The Devourer Below" [] [] difficulty)
        { scenarioLocationLayout = Just
          [ "woods1     .     woods2"
          , "woods1 mainPath woods2"
          , "woods3 mainPath woods4"
          , "woods3 ritualSite woods4"
          , "   .   ritualSite   .  "
          ]
        }

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount env [Trait]) => HasTokenValue env ReturnToTheDevourerBelow where
  getTokenValue (ReturnToTheDevourerBelow theDevourerBelow') iid =
    getTokenValue theDevourerBelow' iid

instance (HasId (Maybe LocationId) env LocationMatcher, ScenarioRunner env) => RunMessage env ReturnToTheDevourerBelow where
  runMessage msg s@(ReturnToTheDevourerBelow theDevourerBelow'@(TheDevourerBelow attrs))
    = case msg of
      Setup -> do
        investigatorIds <- getInvestigatorIds
        pastMidnight <- getHasRecord ItIsPastMidnight
        ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
        cultistsWhoGotAway <- getRecordSet CultistsWhoGotAway
        ghoulPriestCard <- lookupEncounterCard "01116" <$> getRandom
        mainPathId <- getRandom
        let
          arkhamWoods =
            [ "01150"
            , "01151"
            , "01152"
            , "01153"
            , "01154"
            , "01155"
            , "50033"
            , "50034"
            , "50035"
            , "50036"
            ]
          woodsLabels = ["woods1", "woods2", "woods3", "woods4"]
          ghoulPriestMessages =
            [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
          pastMidnightMessages =
            if pastMidnight then [AllRandomDiscard, AllRandomDiscard] else []
          cultistsWhoGotAwayMessages = replicate
            ((length cultistsWhoGotAway + 1) `div` 2)
            PlaceDoomOnAgenda
        woodsLocations <-
          zip <$> getRandoms <*> (take 4 <$> shuffleM arkhamWoods)
        randomSet <-
          sample
          $ EncounterSet.AgentsOfYogSothoth
          :| [ EncounterSet.AgentsOfShubNiggurath
             , EncounterSet.AgentsOfCthulhu
             , EncounterSet.AgentsOfHastur
             ]
        encounterDeck <- buildEncounterDeck
          [ EncounterSet.ReturnToTheDevourerBelow
          , EncounterSet.TheDevourerBelow
          , EncounterSet.AncientEvils
          , EncounterSet.StrikingFear
          , EncounterSet.GhoulsOfUmordhoth
          , EncounterSet.TheDevourersCult
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
            , PlaceLocation "01149" mainPathId
            ]
          <> [ PlaceLocation cardCode locationId
             | (locationId, cardCode) <- woodsLocations
             ]
          <> [ SetLocationLabel locationId label
             | (label, (locationId, _)) <- zip woodsLabels woodsLocations
             ]
          <> [RevealLocation Nothing mainPathId, MoveAllTo mainPathId]
          <> ghoulPriestMessages
          <> cultistsWhoGotAwayMessages
          <> pastMidnightMessages
        let
          locations' = mapFromList $ map
            (second pure . toFst (getLocationName . lookupLocationStub))
            (["01149", "01156"] <> map snd woodsLocations)
        ReturnToTheDevourerBelow . TheDevourerBelow <$> runMessage
          msg
          (attrs & locationsL .~ locations')
      CreateEnemyAt card lid _ | toCardCode card == "01157" -> do
        name <- getName lid
        if name == "Ritual Site"
          then do
            vaultOfEarthlyDemise <- EncounterCard <$> genEncounterCard "50032b"
            s <$ unshiftMessage
              (AttachStoryTreacheryTo
                vaultOfEarthlyDemise
                (CardCodeTarget "00157")
              )
          else pure s
      _ -> ReturnToTheDevourerBelow <$> runMessage msg theDevourerBelow'
