module Arkham.Types.Scenario.Scenarios.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.CurtainCall.Story
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet qualified as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import Arkham.Types.Token

newtype CurtainCall = CurtainCall ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: Difficulty -> CurtainCall
curtainCall difficulty =
  CurtainCall
    $ baseAttrs "03043" "Curtain Call" difficulty
    & locationLayoutL
    ?~ [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
       , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
       , "lobbyDoorway2 .     .       .         backstageDoorway2"
       ]

instance HasRecord env CurtainCall where
  hasRecord _ _ = pure False
  hasRecordSet _ _ = pure []
  hasRecordCount _ _ = pure 0

instance
  ( HasTokenValue env InvestigatorId
  , HasCount HorrorCount env InvestigatorId
  )
  => HasTokenValue env CurtainCall where
  getTokenValue (CurtainCall attrs) iid = \case
    Skull -> do
      horrorCount <- unHorrorCount <$> getCount iid
      let easyStandardModifier = if horrorCount >= 3 then 3 else 1
      let hardExpertModifier = max 1 horrorCount
      pure $ toTokenValue attrs Skull easyStandardModifier hardExpertModifier
    face | face `elem` [Cultist, Tablet, ElderThing] ->
      pure $ toTokenValue attrs face 4 5
    otherFace -> getTokenValue attrs iid otherFace

instance ScenarioRunner env => RunMessage env CurtainCall where
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
        lid <- getId iid
        horrorCount <- unHorrorCount <$> getCount lid
        s <$ push
          (if horrorCount > 0
            then InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
            else PlaceHorror (LocationTarget lid) 1
          )
    _ -> CurtainCall <$> runMessage msg attrs
