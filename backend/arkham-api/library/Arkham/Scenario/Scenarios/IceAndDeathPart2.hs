module Arkham.Scenario.Scenarios.IceAndDeathPart2 (IceAndDeathPart2 (..), iceAndDeathPart2) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosBag (hasRemainingFrostTokens)
import Arkham.Helpers.Investigator (getMaybeLocation, withLocationOf)
import Arkham.Helpers.Log (getRecordSet, whenHasRecord)
import Arkham.Helpers.Query (getLead, getPlayerCount, getSetAsideCard)
import Arkham.Helpers.Xp (toBonus)
import Arkham.I18n
import Arkham.Location.Types qualified as Location
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Uncharted))

newtype IceAndDeathPart2 = IceAndDeathPart2 ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceAndDeathPart2 :: Difficulty -> IceAndDeathPart2
iceAndDeathPart2 difficulty =
  scenarioWith
    IceAndDeathPart2
    "08501b"
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
    (referenceL .~ "08501")

instance HasChaosTokenValue IceAndDeathPart2 where
  getChaosTokenValue iid tokenFace (IceAndDeathPart2 attrs) = case tokenFace of
    Skull -> do
      n <-
        fromMaybe 0 <$> runMaybeT do
          lid <- MaybeT $ getMaybeLocation iid
          MaybeT $ shelterValue lid
      pure $ toChaosTokenValue attrs Skull ((n + 1) `div` 2) n
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

possessedMap :: Map CardCode CardDef
possessedMap =
  mapFromList
    [
      ( Assets.professorWilliamDyerProfessorOfGeology.cardCode
      , Enemies.professorWilliamDyerProfessorOfGeology
      )
    , (Assets.danforthBrilliantStudent.cardCode, Enemies.danforthBrilliantStudent)
    , (Assets.eliyahAshevakDogHandler.cardCode, Enemies.eliyahAshevakDogHandler)
    , (Assets.drMalaSinhaDaringPhysician.cardCode, Enemies.drMalaSinhaDaringPhysician)
    , (Assets.averyClaypoolAntarcticGuide.cardCode, Enemies.averyClaypoolAntarcticGuide)
    , (Assets.jamesCookieFredericksDubiousChoice.cardCode, Enemies.jamesCookieFredericksDubiousChoice)
    , (Assets.drAmyKenslerProfessorOfBiology.cardCode, Enemies.drAmyKenslerProfessorOfBiology)
    , (Assets.roaldEllsworthIntrepidExplorer.cardCode, Enemies.roaldEllsworthIntrepidExplorer)
    , (Assets.takadaHirokoAeroplaneMechanic.cardCode, Enemies.takadaHirokoAeroplaneMechanic)
    ]

stories :: [CardDef]
stories =
  [ Stories.deadEnd
  , Stories.cracksInTheIce
  , Stories.somberRemains
  , Stories.disappearingFootprints
  , Stories.dissectedExplorer
  , Stories.evilWithin
  , Stories.bloodyEvidence
  , Stories.madnessInside
  ]

instance RunMessage IceAndDeathPart2 where
  runMessage msg s@(IceAndDeathPart2 attrs) = runQueueT $ scenarioI18n 2 $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "iceAndDeath2"
      whenM hasRemainingFrostTokens $ addChaosToken #frost

      kensler <- getPartner Assets.drAmyKenslerProfessorOfBiology
      when (kensler.status `notElem` [Eliminated, Mia]) do
        story $ i18nWithTitle "kenslerAliveAndNotMissing"
        sinha <- getPartner Assets.drMalaSinhaDaringPhysician
        storyWhen (sinha.status == Mia) $ i18nWithTitle "sinhaMissing"

      dyer <- getPartner Assets.professorWilliamDyerProfessorOfGeology
      when (dyer.status `notElem` [Eliminated, Mia]) do
        story $ i18nWithTitle "dyerAliveAndNotMissing"
        danforth <- getPartner Assets.danforthBrilliantStudent
        storyWhen (danforth.status == Mia) $ i18nWithTitle "danforthMissing"

      claypool <- getPartner Assets.averyClaypoolAntarcticGuide
      when (claypool.status `notElem` [Eliminated, Mia]) do
        story $ i18nWithTitle "claypoolAliveAndNotMissing"
        ellsworth <- getPartner Assets.roaldEllsworthIntrepidExplorer
        storyWhen (ellsworth.status == Mia) $ i18nWithTitle "ellsworthMissing"

      fredericks <- getPartner Assets.jamesCookieFredericksDubiousChoice
      when (fredericks.status `notElem` [Eliminated, Mia]) do
        story $ i18nWithTitle "fredericksAliveAndNotNotMissing"
        takada <- getPartner Assets.takadaHirokoAeroplaneMechanic
        storyWhen (takada.status == Mia) $ i18nWithTitle "takadaMissing"

      eachInvestigator (`forInvestigator` PreScenarioSetup)
      pure s
    ForInvestigator iid PreScenarioSetup -> do
      getCurrentShelterValue >>= traverse_ \sv -> do
        setupModifier ScenarioSource iid (BaseStartingResources sv)

      kensler <- getPartner Assets.drAmyKenslerProfessorOfBiology
      sinha <- getPartner Assets.drMalaSinhaDaringPhysician

      dyer <- getPartner Assets.professorWilliamDyerProfessorOfGeology
      danforth <- getPartner Assets.danforthBrilliantStudent

      claypool <- getPartner Assets.averyClaypoolAntarcticGuide
      ellsworth <- getPartner Assets.roaldEllsworthIntrepidExplorer

      fredericks <- getPartner Assets.jamesCookieFredericksDubiousChoice
      takada <- getPartner Assets.takadaHirokoAeroplaneMechanic

      let
        mustTake =
          [kensler | kensler.status `notElem` [Eliminated, Mia] && sinha.status == Mia]
            <> [dyer | dyer.status `notElem` [Eliminated, Mia] && danforth.status == Mia]
            <> [claypool | claypool.status `notElem` [Eliminated, Mia] && ellsworth.status == Mia]
            <> [fredericks | fredericks.status `notElem` [Eliminated, Mia] && takada.status == Mia]

      n <- getPlayerCount
      takenCount <- selectCount $ mapOneOf assetIs $ toList expeditionTeam
      remainingPartners <- getRemainingPartners

      let partners = if length mustTake >= n - takenCount then mustTake else remainingPartners

      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          when (length mustTake < n - takenCount) do
            labeled "Do not take a partner" nothing
          for_ partners \partner -> do
            inPlay <- selectAny $ assetIs partner.cardCode
            when (not inPlay) do
              cardLabeled partner.cardCode $ handleTarget iid ScenarioSource (CardCodeTarget partner.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        when
          ( cardCode
              `elem` [ Assets.drAmyKenslerProfessorOfBiology.cardCode
                     , Assets.professorWilliamDyerProfessorOfGeology.cardCode
                     , Assets.averyClaypoolAntarcticGuide.cardCode
                     , Assets.jamesCookieFredericksDubiousChoice.cardCode
                     ]
          )
          do
            setupModifier ScenarioSource card (AdditionalStartingUses 1)

        assetId <- createAssetAt card (InPlayArea iid)
        partner <- getPartner cardCode
        pushWhen (partner.damage > 0) $ Msg.PlaceDamage CampaignSource (toTarget assetId) partner.damage
        pushWhen (partner.horror > 0) $ Msg.PlaceHorror CampaignSource (toTarget assetId) partner.horror
      pure s
    Setup -> runScenarioSetup IceAndDeathPart2 attrs do
      gather Set.IceAndDeath
      gather Set.LostInTheNight
      gather Set.LeftBehind
      gather Set.DeadlyWeather
      gather Set.HazardsOfAntarctica
      gather Set.SilenceAndMystery
      gather Set.Tekelili
      gather Set.AncientEvils

      revealed <- getRecordSet LocationsRevealed
      for_ (mapToList camps) \(cardCode, camp) -> do
        amongGathered (CardWithCardCode cardCode) >>= \case
          [card] -> do
            location <- place (toCardDef card)
            when (recorded cardCode `elem` revealed) $ reveal location
            whenHasRecord camp $ startAt location
          _ -> error $ "Expected exactly one location for card code: " <> show cardCode

      doStep 2 msg

      setAgendaDeck [Agendas.aHarshWindBlows, Agendas.theChillOfNight, Agendas.madnessAndDeath]
      setActDeck [Acts.theLostExpedition]

      mia <- mapMaybe (\p -> lookup p.cardCode possessedMap) <$> getPartnersWithStatus (== Mia)
      setAside $ mia <> stories

      addTekeliliDeck
    DoStep 2 Setup -> do
      mia <- mapMaybe (\p -> lookup p.cardCode possessedMap) <$> getPartnersWithStatus (== Mia)
      (cards, removals) <- splitAt 9 <$> (shuffle . (mia <>) =<< shuffle stories)
      locations <- select $ LocationWithTrait Uncharted

      for_ (zip cards locations) \(def, location) -> do
        card <- getSetAsideCard def
        obtainCard card
        placeUnderneath location [card]

      for_ removals (getSetAsideCard >=> obtainCard)

      pure s
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
      locations <- selectField Location.LocationCardCode RevealedLocation
      recordSetInsert LocationsRevealed locations
      endOfScenario
      pure s
    Resign iid -> do
      withLocationOf iid \lid -> do
        card <- field Location.LocationCard lid
        for_ (lookup card.cardCode camps) record
      pure s
    _ -> IceAndDeathPart2 <$> liftRunMessage msg attrs
