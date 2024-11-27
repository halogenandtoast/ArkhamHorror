module Arkham.Scenario.Scenarios.IceAndDeath (IceAndDeath (..), iceAndDeath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator (getMaybeLocation, withLocationOf)
import Arkham.Helpers.Log (getCampaignLog)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp (toBonus)
import Arkham.I18n
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types qualified as Location
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Trait (Trait (Uncharted))

newtype IceAndDeath = IceAndDeath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceAndDeath :: Difficulty -> IceAndDeath
iceAndDeath difficulty =
  scenario
    IceAndDeath
    "08501"
    "Ice and Death"
    difficulty
    [ "trefoil  .       .     .         .        .         plus"
    , "trefoil  .       .     moon      .        .         plus"
    , ".        droplet .     moon      .        equals    ."
    , ".        droplet .     .         .        equals    ."
    , ".        .       heart .         triangle .         ."
    , ".        .       heart .         triangle .         ."
    , ".        .       .     circle    .        .         ."
    , ".        star    .     circle    .        hourglass ."
    , ".        star    .     diamond   .        hourglass ."
    , ".        .       .     diamond   .        .         ."
    , ".        .       .     square    .        .         ."
    , ".        .       .     square    .        .         ."
    , ".        .       .     squiggle  .        .         ."
    , ".        .       .     squiggle  .        .         ."
    ]

instance HasChaosTokenValue IceAndDeath where
  getChaosTokenValue iid tokenFace (IceAndDeath attrs) = case tokenFace of
    Skull -> do
      n <-
        fromMaybe 0 <$> runMaybeT do
          lid <- MaybeT $ getMaybeLocation iid
          MaybeT $ shelterValue lid
      pure $ toChaosTokenValue attrs Skull ((n + 1) `div` 2) n
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage IceAndDeath where
  runMessage msg s@(IceAndDeath attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeath"
      doStep 1 PreScenarioSetup
      eachInvestigator (`forInvestigator` PreScenarioSetup)
      pure s
    DoStep 1 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro1"

      selectAny (investigatorIs Investigators.winifredHabbamock) >>= \case
        True -> doStep 2 PreScenarioSetup
        False -> doStep 3 PreScenarioSetup

      pure s
    DoStep 2 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro2"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro3"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeathPart1Intro4"
      killed <- sample expeditionTeam
      push $ SetPartnerStatus killed.cardCode Eliminated
      recordSetInsert WasKilledInThePlaneCrash [killed.cardCode]
      if
        | killed == Assets.professorWilliamDyerProfessorOfGeology ->
            story $ i18n "williamDyerKilledInPlaneCrash"
        | killed == Assets.roaldEllsworthIntrepidExplorer -> story $ i18n "roaldEllsworthKilledInPlaneCrash"
        | killed == Assets.eliyahAshevakDogHandler -> story $ i18n "eliyahAshevakKilledInPlaneCrash"
        | killed == Assets.danforthBrilliantStudent -> story $ i18n "danforthKilledInPlaneCrash"
        | killed == Assets.jamesCookieFredericksDubiousChoice ->
            story $ i18n "jamesFredericksKilledInPlaneCrash"
        | killed == Assets.averyClaypoolAntarcticGuide -> story $ i18n "averyClaypoolKilledInPlaneCrash"
        | killed == Assets.takadaHirokoAeroplaneMechanic -> story $ i18n "takadaHirokoKilledInPlaneCrash"
        | killed == Assets.drMalaSinhaDaringPhysician -> story $ i18n "malaSinhaKilledInPlaneCrash"
        | killed == Assets.drAmyKenslerProfessorOfBiology -> story $ i18n "amyKenslerKilledInPlaneCrash"
        | otherwise -> error "Invalid card in expedition team"

      pure s
    ForInvestigator iid PreScenarioSetup -> do
      partners <- view partnersL <$> getCampaignLog
      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          labeled "Do not take a partner" nothing
          for_ (mapToList partners) \(cardCode, partner) -> do
            inPlay <- selectAny $ assetIs cardCode
            when (not inPlay && partner.status `elem` [Safe, Resolute]) do
              cardLabeled cardCode $ handleTarget iid ScenarioSource (CardCodeTarget cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        assetId <- createAssetAt card (InPlayArea iid)
        partners <- view partnersL <$> getCampaignLog
        for_ (lookup cardCode partners) \partner -> do
          pushWhen (partner.damage > 0) $ Msg.PlaceDamage CampaignSource (toTarget assetId) partner.damage
          pushWhen (partner.horror > 0) $ Msg.PlaceHorror CampaignSource (toTarget assetId) partner.horror
      pure s
    Setup -> runScenarioSetup IceAndDeath attrs do
      gather Set.IceAndDeath
      gather Set.TheCrash
      gather Set.DeadlyWeather
      gather Set.HazardsOfAntarctica
      gather Set.SilenceAndMystery
      gather Set.Tekelili
      gather Set.AncientEvils

      gatherAndSetAside Set.CreaturesInTheIce

      placeAll [Locations.precariousIceSheet, Locations.treacherousPath, Locations.frozenShores]
      startAt =<< place Locations.crashSite

      setAside
        =<< amongGathered
          ( oneOf
              [ #location <> CardWithTrait Uncharted
              , cardIs Enemies.skitteringNonsense
              , cardIs Enemies.terrorOfTheStarsBringerOfIceAndDeath
              ]
          )

      setAgendaDeck [Agendas.coldWelcome, Agendas.intoTheWhite, Agendas.runningOutOfTime]
      setActDeck [Acts.searchForACampSite]

      addTekeliliDeck

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist -> do
          let x = if isEasyStandard attrs then 1 else n
          tekelili <- take x <$> getScenarioDeck TekeliliDeck
          canModifyDeck <- can.manipulate.deck iid
          if null tekelili || not canModifyDeck
            then assignHorror iid Cultist x
            else do
              addTekelili iid tekelili
              when (length tekelili < x) $ assignHorror iid Cultist (x - length tekelili)
        Tablet -> push $ DiscardTopOfDeck iid n (toSource Tablet) (Just $ toTarget attrs)
        _ -> pure ()
      pure s
    DiscardedTopOfDeck iid cards (isSource Tablet -> True) (isTarget attrs -> True) -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) cards
      when (notNull weaknesses) $ addToHand iid weaknesses
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          locations <- selectWithField Location.LocationCard LocationWithoutClues

          if null locations
            then do
              record Camp_CrashSite
              allGainXpWithBonus attrs $ toBonus "bonus" 3
            else do
              lead <- getLead
              chooseOneM lead do
                for_ locations \(location, card) -> do
                  for_ (lookup card.cardCode camps) \camp ->
                    targeting location do
                      record camp
                      allGainXpWithBonus attrs $ toBonus "bonus" (max 3 $ fromMaybe 0 $ getShelterValue card)
        Resolution 1 -> do
          story $ i18nWithTitle "resolution1"
          sv <- max 3 . fromMaybe 0 <$> getCurrentShelterValue
          allGainXpWithBonus attrs $ toBonus "bonus" sv
        _ -> error "Unknown resolution"
      endOfScenario
      pure s
    Resign iid -> do
      withLocationOf iid \lid -> do
        card <- field Location.LocationCard lid
        for_ (lookup card.cardCode camps) record
      pure s
    _ -> IceAndDeath <$> liftRunMessage msg attrs
