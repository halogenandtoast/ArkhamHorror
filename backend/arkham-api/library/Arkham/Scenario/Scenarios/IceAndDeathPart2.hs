module Arkham.Scenario.Scenarios.IceAndDeathPart2 (iceAndDeathPart2) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLog hiding (optionsL)
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Capability
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.FlavorText
import Arkham.Helpers.ChaosBag (hasRemainingFrostTokens)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCard, allInvestigators)
import Arkham.Helpers.Text
import Arkham.Location.Types qualified as Location
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Uncharted))
import Data.Bitraversable (bimapM)
import Data.Map.Strict qualified as Map

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
    iceAndDeathLayout
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
      isStandalone <- getIsStandalone
      if isStandalone
        then do
          if attrs.hasOption ManuallyPickKilledInPlaneCrash
            then do
              let addPartner partner =
                    standaloneCampaignLogL
                      . partnersL
                      . at partner.cardCode
                      %~ Just
                      . fromMaybe (CampaignLogPartner 0 0 Mia)
              pure $ IceAndDeathPart2 $ foldl' (flip addPartner) attrs expeditionTeam
            else do
              (killed, mia) <- sampleWithRest expeditionTeam
              let addPartner partner status = standaloneCampaignLogL . partnersL . at partner.cardCode ?~ CampaignLogPartner 0 0 status
              pure
                $ IceAndDeathPart2
                $ foldl' (flip (uncurry addPartner)) attrs ((killed, Eliminated) :| map (,Mia) mia)
        else do
          story $ i18nWithTitle "intro"
          whenM hasRemainingFrostTokens $ addChaosToken #frost

          kensler <- getPartner Assets.drAmyKenslerProfessorOfBiology
          unless (kensler.status `elem` [Eliminated, Mia]) do
            sinha <- getPartner Assets.drMalaSinhaDaringPhysician
            blueStory
              $ i18nEntry "kenslerAliveAndNotMissing"
              <> validateEntry (sinha.status == Mia) "sinhaMissing"

          dyer <- getPartner Assets.professorWilliamDyerProfessorOfGeology
          unless (dyer.status `elem` [Eliminated, Mia]) do
            danforth <- getPartner Assets.danforthBrilliantStudent
            blueStory
              $ i18nEntry "dyerAliveAndNotMissing"
              <> validateEntry (danforth.status == Mia) "danforthMissing"

          claypool <- getPartner Assets.averyClaypoolAntarcticGuide
          unless (claypool.status `elem` [Eliminated, Mia]) do
            ellsworth <- getPartner Assets.roaldEllsworthIntrepidExplorer
            blueStory
              $ i18nEntry "claypoolAliveAndNotMissing"
              <> validateEntry (ellsworth.status == Mia) "ellsworthMissing"

          fredericks <- getPartner Assets.jamesCookieFredericksDubiousChoice
          unless (fredericks.status `elem` [Eliminated, Mia]) do
            takada <- getPartner Assets.takadaHirokoAeroplaneMechanic
            blueStory
              $ i18nEntry "fredericksAliveAndNotMissing"
              <> validateEntry (takada.status == Mia) "takadaMissing"

          sv <- fromMaybe 0 <$> getCurrentShelterValue
          story $ withVars ["shelterValue" .= sv] $ i18nWithTitle "investigatorSetup"
          investigators <- zip [0..] <$> allInvestigators
          for_ investigators \(idx, investigator) ->
            forInvestigator investigator (DoStep idx PreScenarioSetup)
          pure s
    ForInvestigator iid (DoStep idx PreScenarioSetup) -> do
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

      let remainingChoices = n - idx
      needsToBeTaken <- filterM (selectNone . assetIs) mustTake
      remainingPartners <- getRemainingPartners

      let partners = if length needsToBeTaken >= remainingChoices then needsToBeTaken else remainingPartners

      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          when (length needsToBeTaken < remainingChoices) do
            labeled "Do not take a partner" nothing
          for_ partners \partner -> do
            inPlay <- selectAny $ assetIs partner.cardCode
            unless inPlay do
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
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      mCamp <- getCamp
      when (isNothing mCamp) do
        for_ (nonEmpty $ Map.elems camps) (sample >=> record)

      pure s
    Setup -> runScenarioSetup IceAndDeathPart2 attrs do
      scope "setup" $ story $ flavorText $ ul do
        li "gatherSets"
        li "buildAgendaDecks"
        li.nested "placeLocations" do
          li "unrevealed"
        li.nested "missing" do
          li "findPossessed"
          li "findStory"
          li "shuffleTogether"
        li "beginAtCamp"
        li "tekelili"
        unscoped do
          li "shuffleRemainder"
          li "readyToBegin"

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

      partners <- getPartnersWithStatus (const True) -- probably should extract a helper for this
      let toEnemies = mapMaybe (\x -> lookup x.cardCode possessedMap)
      let (mia, rest) = bimap toEnemies toEnemies $ partition (\x -> x.status == Mia) partners
      removeEvery rest
      setAside $ mia <> stories

      addTekeliliDeck
    DoStep 2 Setup -> do
      mia <- mapMaybe (\x -> lookup x.cardCode possessedMap) <$> getPartnersWithStatus (== Mia)
      (cards, removals) <- bimapM shuffle pure . splitAt 9 . (mia <>) =<< shuffle stories
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          storyWithChooseOneM (i18nWithTitle "noResolution") do
            labeled "Proceed to _Resolution 1_" $ push R1
        Resolution 1 -> do
          baseVictory <- allGainXp' attrs
          story
            $ withVars ["xp" .= baseVictory]
            $ i18nWithTitle "resolution1"
          partners <- getPartnersWithStatus (== Mia) -- If killed the status will be eliminated
          for_ partners \partner -> do
            for_ (Map.lookup partner.cardCode possessedMap) \enemy -> do
              inVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ CardWithCardCode enemy.cardCode
              setPartnerStatus partner $ if inVictory then Safe else Eliminated

          locations <- selectField Location.LocationCardCode RevealedLocation
          recordSetInsert LocationsRevealed locations
          endOfScenario
        _ -> error "Unknown resolution"
      pure s
    HandleOption option -> do
      whenM getIsStandalone do
        case option of
          ManuallyPickCamp -> pure ()
          ManuallyPickKilledInPlaneCrash -> pure ()
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> IceAndDeathPart2 <$> liftRunMessage msg attrs
