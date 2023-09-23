module Arkham.Scenario.Scenarios.InTheClutchesOfChaos (
  InTheClutchesOfChaos (..),
  inTheClutchesOfChaos,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Scenarios.InTheClutchesOfChaos.Story
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype InTheClutchesOfChaos = InTheClutchesOfChaos ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheClutchesOfChaos :: Difficulty -> InTheClutchesOfChaos
inTheClutchesOfChaos difficulty =
  scenario
    InTheClutchesOfChaos
    "05284"
    "In the Clutches of Chaos"
    difficulty
    [ ".            .            .      merchantDistrict merchantDistrict rivertown   rivertown  .          .                   ."
    , "hangmansHill hangmansHill uptown uptown           southside        southside   frenchHill frenchHill silverTwilightLodge silverTwilightLodge"
    , ".            .            .      .                southChurch      southChurch .          .          .                   ."
    ]

instance HasChaosTokenValue InTheClutchesOfChaos where
  getChaosTokenValue iid tokenFace (InTheClutchesOfChaos attrs) = case tokenFace of
    Skull -> do
      doom <- getSum <$> selectAgg Sum LocationDoom (locationWithInvestigator iid)
      breaches <-
        sum . map (maybe 0 countBreaches) <$> selectFields LocationBreaches (locationWithInvestigator iid)
      pure $ toChaosTokenValue attrs Skull (doom + breaches) (doom + breaches + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage InTheClutchesOfChaos where
  runMessage msg s@(InTheClutchesOfChaos attrs) = case msg of
    PreScenarioSetup -> do
      investigators <- allInvestigators
      neverSeenOrHeardFromAgain <- getHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain
      pushAll
        $ [story investigators intro1]
        <> [story investigators intro2 | neverSeenOrHeardFromAgain]
        <> [story investigators intro3 | not neverSeenOrHeardFromAgain]
        <> [story investigators intro4]
      pure s
    StandaloneSetup -> do
      lead <- getLead
      pushAll
        [ chooseOne
            lead
            [ Label "Anette Mason is possessed by evil." [Record AnetteMasonIsPossessedByEvil]
            , Label
                "Carl Sanford possesses the secrets of the universe."
                [Record CarlSanfordPossessesTheSecretsOfTheUniverse]
            ]
        ]
      pure s
    Setup -> do
      anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
      carlSanfordPossessesTheSecretsOfTheUniverse <-
        getHasRecord CarlSanfordPossessesTheSecretsOfTheUniverse
      gatheredCards <-
        buildEncounterDeckExcluding [Enemies.piperOfAzathoth]
          $ [ EncounterSet.InTheClutchesOfChaos
            , EncounterSet.AgentsOfAzathoth
            , EncounterSet.Nightgaunts
            ]
          <> ( guard anetteMasonIsPossessedByEvil
                *> [ EncounterSet.MusicOfTheDamned
                   , EncounterSet.AnettesCoven
                   , EncounterSet.CityOfSins
                   , EncounterSet.Witchcraft
                   ]
             )
          <> ( guard carlSanfordPossessesTheSecretsOfTheUniverse
                *> [ EncounterSet.SecretsOfTheUniverse
                   , EncounterSet.SilverTwilightLodge
                   , EncounterSet.StrikingFear
                   ]
             )

      midnightMasks <-
        traverse
          genEncounterCard
          [ Treacheries.huntingShadow
          , Treacheries.huntingShadow
          , Treacheries.huntingShadow
          , Treacheries.falseLead
          , Treacheries.falseLead
          ]

      encounterDeck <-
        Deck
          <$> shuffleM
            ( unDeck gatheredCards <> (guard carlSanfordPossessesTheSecretsOfTheUniverse *> midnightMasks)
            )

      frenchHill <- sample $ Locations.frenchHill_290 :| [Locations.frenchHill_291]
      rivertown <- sample $ Locations.rivertown_292 :| [Locations.rivertown_293]
      southside <- sample $ Locations.southside_294 :| [Locations.southside_295]
      uptown <- sample $ Locations.uptown_296 :| [Locations.uptown_297]
      southChurch <- sample $ Locations.southChurch_298 :| [Locations.southChurch_299]
      merchantDistrict <- sample $ Locations.merchantDistrict_300 :| [Locations.merchantDistrict_301]

      (southsideId, placeSouthside) <- placeLocationCard southside
      placeRest <- placeLocationCards_ [frenchHill, rivertown, uptown, southChurch, merchantDistrict]

      placeHangmansHill <-
        placeLocationCard_
          $ if anetteMasonIsPossessedByEvil
            then Locations.hangmansHillWhereItAllEnds
            else Locations.hangmansHillShroudedInMystery

      placeSilverTwilightLodge <-
        placeLocationCard_
          $ if anetteMasonIsPossessedByEvil
            then Locations.silverTwilightLodgeShroudedInMystery
            else Locations.silverTwilightLodgeWhereItAllEnds

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeSouthside
          , MoveAllTo (toSource attrs) southsideId
          , placeHangmansHill
          , placeSilverTwilightLodge
          ]
        <> placeRest
        <> [SetupStep (toTarget attrs) 1]

      agendas <- genCards [Agendas.theChariotVII]

      acts <-
        genCards
          $ if anetteMasonIsPossessedByEvil
            then [Acts.darkKnowledgeV1, Acts.beyondTheGrave]
            else [Acts.darkKnowledgeV2, Acts.newWorldOrder]

      setAsideCards <- genCards [Enemies.piperOfAzathoth]

      InTheClutchesOfChaos
        <$> runMessage
          msg
          ( attrs
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
              & (setAsideCardsL .~ setAsideCards)
          )
    SetupStep (isTarget attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      if playerCount == 4
        then replicateM_ 3 $ do
          lids <- sampleLocations 3
          pushAll [PlaceBreaches (toTarget lid) 1 | lid <- lids]
        else replicateM_ playerCount $ do
          lids <- sampleLocations 2
          pushAll [PlaceBreaches (toTarget lid) 1 | lid <- lids]
      pure s
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      mLocation <- selectOne $ locationWithInvestigator iid
      for_ mLocation $ \location -> do
        n <- getBreaches location
        pushWhen (n < 3) $ PlaceBreaches (toTarget location) 1
      pure s
    FailedSkillTest _iid _ _ (ChaosTokenTarget token) _ n -> do
      act <- selectJust AnyAct
      case chaosTokenFace token of
        Tablet -> push $ RemoveBreaches (toTarget act) n
        ElderThing -> do
          lid <- sampleLocation
          push $ PlaceBreaches (toTarget lid) 1
        _ -> pure ()
      pure s
    ScenarioResolution n -> do
      investigators <- allInvestigatorIds
      lead <- getLead
      gainXp <- toGainXp (toSource attrs) getXp
      anyDetectivePoliceOrAgency <-
        selectAny
          $ AnyInvestigator
          $ map InvestigatorWithTrait [Trait.Detective, Trait.Police, Trait.Agency]
      case n of
        NoResolution -> do
          anetteMasonIsPossessedByEvil <- getHasRecord AnetteMasonIsPossessedByEvil
          push $ if anetteMasonIsPossessedByEvil then R3 else R4
        Resolution 1 -> do
          anySorcererMiskatonicOrScholar <-
            selectAny
              $ AnyInvestigator
              $ map InvestigatorWithTrait [Trait.Sorcerer, Trait.Miskatonic, Trait.Scholar]
          pushAll
            $ [ story investigators resolution1
              , chooseOne lead
                  $ [ Label
                        "“You’ve done enough harm. We’ll handle this from here.”"
                        [Record TheInvestigatorsContinuedAlone]
                    , Label "“We will need your help to fix this.” " [Record TheInvestigatorsAskedAnetteForAssistance]
                    ]
                  <> [ Label "“You are under arrest.”" [Record TheInvestigatorsArrestedAnette]
                     | anyDetectivePoliceOrAgency
                     ]
                  <> [ Label "“Then teach me how to be stronger.”" [Record AnetteTaughtYouTheSpellsOfOld]
                     | anySorcererMiskatonicOrScholar
                     ]
              ]
            <> gainXp
            <> [EndOfGame Nothing]
        Resolution 2 -> do
          anySorcererSilverTwilightOrCultist <-
            selectAny
              $ AnyInvestigator
              $ map InvestigatorWithTrait [Trait.Sorcerer, Trait.SilverTwilight, Trait.Cultist]
          pushAll
            $ [ story investigators resolution2
              , chooseOne lead
                  $ [ Label
                        "“You’ve done enough harm. We’ll handle this from here.”"
                        [Record TheInvestigatorsContinuedAlone]
                    , Label "“We will need your help to fix this.” " [Record TheInvestigatorsAskedSanfordForAssistance]
                    ]
                  <> [ Label "“You are under arrest.”" [Record TheInvestigatorsArrestedSanford]
                     | anyDetectivePoliceOrAgency
                     ]
                  <> [ Label
                      "“Then teach me how to be stronger.”"
                      [Record TheInvestigatorsAssumedControlOfTheSilverTwilightLodge]
                     | anySorcererSilverTwilightOrCultist
                     ]
              ]
            <> gainXp
            <> [EndOfGame Nothing]
        Resolution 3 ->
          pushAll
            $ [story investigators resolution3, Record DoomDrawsEverCloser]
            <> gainXp
            <> [EndOfGame Nothing]
        Resolution 4 ->
          pushAll
            $ [story investigators resolution4, Record DoomDrawsEverCloser]
            <> gainXp
            <> [EndOfGame Nothing]
        _ -> error "no such resolution"
      pure s
    _ -> InTheClutchesOfChaos <$> runMessage msg attrs
