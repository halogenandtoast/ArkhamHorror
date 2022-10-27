module Arkham.Scenario.Scenarios.TheCityOfArchives
  ( TheCityOfArchives(..)
  , theCityOfArchives
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheCityOfArchives.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait hiding ( Trait (Cultist) )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Control.Lens ( over )

newtype TheCityOfArchives = TheCityOfArchives ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityOfArchives :: Difficulty -> TheCityOfArchives
theCityOfArchives difficulty = scenario
  TheCityOfArchives
  "04237"
  "The City of Archives"
  difficulty
  ["interviewRoom1", "interviewRoom2", "interviewRoom3"]

instance HasTokenValue TheCityOfArchives where
  getTokenValue iid tokenFace (TheCityOfArchives attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheCityOfArchives where
  runMessage msg s@(TheCityOfArchives attrs) = case msg of
    CheckWindow _ [Window Timing.When (Window.DrawingStartingHand iid)] -> do
      uniqueItemAssetCards <-
        selectList $ InDeckOf (InvestigatorWithId iid) <> BasicCardMatch
          (CardWithTrait Item <> CardIsUnique)
      uniqueItemAssets <- selectList $ AssetWithTrait Item <> UniqueAsset

      mAlejandro <-
        selectOne $ InDeckOf (InvestigatorWithId iid) <> BasicCardMatch
          (cardIs Assets.alejandroVela)

      let setAsideUpdate = maybe id (over setAsideCardsL . (:)) mAlejandro

      pushAll
        $ map RemovePlayerCardFromGame uniqueItemAssetCards
        <> [ RemovePlayerCardFromGame alejandro
           | alejandro <- maybeToList mAlejandro
           ]
        <> map (RemoveFromGame . AssetTarget) uniqueItemAssets
      pure . TheCityOfArchives $ attrs & setAsideUpdate
    Setup -> do
      iids <- getInvestigatorIds
      leadInvestigator <- getLeadInvestigatorId

      encounterDeck' <-
        buildEncounterDeckExcluding []
          $ [ EncounterSet.TheCityOfArchives
            , EncounterSet.AgentsOfYogSothoth
            , EncounterSet.LockedDoors
            , EncounterSet.ChillingCold
            , EncounterSet.StrikingFear
            ]
      pushAll
        $ map BecomeYithian iids
        <> [ story iids intro1
           , chooseOne
             leadInvestigator
             [ Label
               "Cooperate and tell the creatures everything you know."
               [ story iids intro2
               , Record TheInvestigatorsCooperatedWithTheYithians
               ]
             , Label
               "Refuse and resist captivity."
               [story iids intro3, Record TheInvestigatorsResistedCaptivity]
             ]
           , SetupStep (toTarget attrs) 1
           ]
      TheCityOfArchives <$> runMessage msg attrs
    SetupStep (isTarget attrs -> True) 1 -> do
      cooperatedWithTheYithians <- getHasRecord
        TheInvestigatorsCooperatedWithTheYithians
      if cooperatedWithTheYithians
        then do
          interviewRoom <- genCard Locations.interviewRoomArrivalChamber
          otherRooms <- traverse genCard =<< shuffleM
            [ Locations.interviewRoomRestrainingChamber
            , Locations.interviewRoomIchorFilledChamber
            ]
          pushAll
            $ [ PlaceLocation interviewRoom
              , SetLocationLabel (toLocationId interviewRoom) "interviewRoom1"
              , MoveAllTo (toSource attrs) (toLocationId interviewRoom)
              ]
            <> map PlaceLocation otherRooms
            <> [ SetLocationLabel
                   (toLocationId l)
                   ("interviewRoom" <> tshow @Int n)
               | (l, n) <- zip otherRooms [1 ..]
               ]
          pure s
        else do
          interviewRoom <- genCard Locations.interviewRoomRestrainingChamber
          otherRooms <- traverse genCard =<< shuffleM
            [ Locations.interviewRoomArrivalChamber
            , Locations.interviewRoomIchorFilledChamber
            ]
          pushAll
            $ [ PlaceLocation interviewRoom
              , SetLocationLabel (toLocationId interviewRoom) "interviewRoom1"
              , MoveAllTo (toSource attrs) (toLocationId interviewRoom)
              ]
            <> map PlaceLocation otherRooms
            <> [ SetLocationLabel
                   (toLocationId l)
                   ("interviewRoom" <> tshow @Int n)
               | (l, n) <- zip otherRooms [1 ..]
               ]
          pure s
    _ -> TheCityOfArchives <$> runMessage msg attrs
