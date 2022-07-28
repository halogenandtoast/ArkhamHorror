module Arkham.Scenario.Scenarios.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.CurtainCall.Story
import Arkham.Target
import Arkham.Token

newtype CurtainCall = CurtainCall ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: Difficulty -> CurtainCall
curtainCall difficulty = scenario
  CurtainCall
  "03043"
  "Curtain Call"
  difficulty
  [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
  , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
  , "lobbyDoorway2 .     .       .         backstageDoorway2"
  ]

instance HasTokenValue CurtainCall where
  getTokenValue iid tokenFace (CurtainCall attrs) = case tokenFace of
    Skull -> do
      horrorCount <- field InvestigatorHorror iid
      let easyStandardModifier = if horrorCount >= 3 then 3 else 1
      let hardExpertModifier = max 1 horrorCount
      pure $ toTokenValue attrs Skull easyStandardModifier hardExpertModifier
    face | face `elem` [Cultist, Tablet, ElderThing] ->
      pure $ toTokenValue attrs face 4 5
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage CurtainCall where
  runMessage msg s@(CurtainCall attrs) = case msg of
    Setup -> do
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.royalEmissary]
        [ EncounterSet.CurtainCall
        , EncounterSet.EvilPortents
        , EncounterSet.Delusions
        , EncounterSet.Hauntings
        , EncounterSet.CultOfTheYellowSign
        , EncounterSet.StrikingFear
        , EncounterSet.Rats
        ]

      theatre <- genCard Locations.theatre
      lobby <- genCard Locations.lobby
      balcony <- genCard Locations.balcony
      backstage <- genCard Locations.backstage

      investigatorIds <- getInvestigatorIds
      mLolaId <- selectOne $ InvestigatorWithTitle "Lola Hayes"
      let
        theatreInvestigatorIds =
          maybe investigatorIds (`deleteFirst` investigatorIds) mLolaId
        theatreMoveTo = map
          (\iid -> MoveTo (toSource attrs) iid (toLocationId theatre))
          theatreInvestigatorIds
        backstageMoveTo =
          [ MoveTo (toSource attrs) lolaId (toLocationId backstage)
          | lolaId <- maybeToList mLolaId
          ]

      pushAll
        ([ story investigatorIds intro
         , SetEncounterDeck encounterDeck
         , SetAgendaDeck
         , SetActDeck
         , PlaceLocation theatre
         , PlaceLocation lobby
         , PlaceLocation balcony
         , PlaceLocation backstage
         ]
        <> theatreMoveTo
        <> backstageMoveTo
        )

      setAsideCards <- traverse
        genCard
        [ Enemies.royalEmissary
        , Enemies.theManInThePallidMask
        , Locations.lightingBox
        , Locations.boxOffice
        , Locations.greenRoom
        , Locations.dressingRoom
        , Locations.rehearsalRoom
        , Locations.trapRoom
        ]

      CurtainCall <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [ Acts.awakening
             , Acts.theStrangerACityAflame
             , Acts.theStrangerThePathIsMine
             , Acts.theStrangerTheShoresOfHali
             , Acts.curtainCall
             ]
          )
        & (agendaStackL . at 1 ?~ [Agendas.theThirdAct, Agendas.encore])
        )
    ScenarioResolution resolution -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      gainXP <- map (uncurry GainXP) <$> getXp
      conviction <- getRecordCount Conviction
      doubt <- getRecordCount Doubt
      let
        stoleFromTheBoxOffice =
          member StoleFromTheBoxOffice (scenarioLog attrs)
      let
        theStrangerIsOnToYou =
          [ Record TheStrangerIsOnToYou
          , AddCampaignCardToDeck
            leadInvestigatorId
            Enemies.theManInThePallidMask
          ]
      s <$ case resolution of
        NoResolution ->
          pushAll
            $ story investigatorIds noResolution
            : theStrangerIsOnToYou
            <> gainXP
            <> [EndOfGame Nothing]
        Resolution 1 -> pushAll
          ([ story investigatorIds resolution1
           , Record YouTriedToWarnThePolice
           , RecordCount Conviction (conviction + 1)
           ]
          <> [ Record ThePoliceAreSuspiciousOfYou | stoleFromTheBoxOffice ]
          <> theStrangerIsOnToYou
          <> gainXP
          <> [EndOfGame Nothing]
          )
        Resolution 2 -> pushAll
          ([ story investigatorIds resolution2
           , Record YouChoseNotToGoToThePolice
           , RecordCount Doubt (doubt + 1)
           ]
          <> [ Record ThePoliceAreSuspiciousOfYou | stoleFromTheBoxOffice ]
          <> theStrangerIsOnToYou
          <> gainXP
          <> [EndOfGame Nothing]
          )
        _ -> error "Invalid resolution"
    ResolveToken _ tokenFace iid
      | tokenFace `elem` [Cultist, Tablet, ElderThing] -> do
        lid <- getJustLocation iid
        horrorCount <- field LocationHorror lid
        s <$ push
          (if horrorCount > 0
            then InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
            else PlaceHorror (LocationTarget lid) 1
          )
    _ -> CurtainCall <$> runMessage msg attrs
