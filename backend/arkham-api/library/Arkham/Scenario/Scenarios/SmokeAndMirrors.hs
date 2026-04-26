module Arkham.Scenario.Scenarios.SmokeAndMirrors (setupSmokeAndMirrors, smokeAndMirrors, SmokeAndMirrors (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.BrethrenOfAsh.Import
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.EncounterCard (lookupEncounterCardDef)
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log (getRecordedCardCodes)
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Id
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.I18n (ikey)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Name (nameTitle)
import Arkham.Helpers (Deck (..))
import Arkham.Matcher (pattern HealableAsset, pattern HealableInvestigator, pattern AnyAsset, pattern AnyCards, pattern Anyone, pattern Anywhere, pattern AssetWithTitle, pattern CardWithTrait, pattern EliteEnemy, pattern EnemyAt, pattern EnemyWithoutTrait, pattern LocationWithCardsUnderneath, enemyIs, investigatorAt)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Trait (Trait (Elite, Humanoid))
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record, recordSetInsert)
import Arkham.Modifier (ModifierType (DoNotTakeUpSlot))
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (getConnectedLocations)
import Arkham.Helpers.SkillTest.Target (withSkillTestEnemyTarget)
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Types (Field (..))
import Arkham.Token
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.SmokeAndMirrors.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype SmokeAndMirrors = SmokeAndMirrors ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

instance HasModifiersFor SmokeAndMirrors where
  getModifiersFor (SmokeAndMirrors attrs) = do
    modifySelect attrs (AssetWithTitle "Dr. Henry Armitage") [DoNotTakeUpSlot #ally]

smokeAndMirrors :: Difficulty -> SmokeAndMirrors
smokeAndMirrors difficulty =
    scenario
        SmokeAndMirrors
        "12133"
        "Smoke and Mirrors"
        difficulty
        [ "northside              downtown          easttown"
        , "miskatonicUniversity   merchantDistrict  waterfrontDistrict"
        , "uptown                 southside         frenchHill"
        ]

instance HasChaosTokenValue SmokeAndMirrors where
  getChaosTokenValue iid chaosTokenFace (SmokeAndMirrors attrs) = case chaosTokenFace of
    Skull -> do
      victoryDisplay <- scenarioFieldMap ScenarioVictoryDisplay (filter ((== EnemyType) . toCardType))
      underAct <- scenarioFieldMap ScenarioCardsUnderActDeck (filter ((== EnemyType) . toCardType))
      let eliteCount = length $ filter (\c -> cardMatch c (CardWithTrait Elite)) (victoryDisplay <> underAct)
      pure $ toChaosTokenValue attrs Skull eliteCount (eliteCount + 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> do
      clues <- field InvestigatorClues iid
      pure $ toChaosTokenValue attrs ElderThing clues (clues * 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

setupSmokeAndMirrors :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupSmokeAndMirrors _attrs = do
  setup $ ul do
    li "gatherSets"
    li "placeLocations"
    li "setOutOfPlay"
    li "checkCampaignLog"
    li "placeDoom"
    li "bearerOfDrHenryArmitage"
    li "setAsideMarkOfElokoss"
    li "shuffleRemainder"
    li "readyToBegin"

  gather Set.SmokeAndMirrors
  gather Set.ArcaneLock
  gather Set.Arkham_C2026
  gather Set.BadWeather
  gather Set.DeadEnds
  gather Set.FlyingTerrors
  gather Set.GangsOfArkham
  gather Set.PeopleOfArkham
  gather Set.Whippoorwills

  downtown <- placeLabeled "downtown" =<< sampleOneOf (Locations.downtownFirstBankOfArkham_c2026, Locations.downtownArkhamSanatorium)
  uptown <- placeLabeled "uptown" =<< sampleOneOf (Locations.uptownStMarrysHospital, Locations.uptownYeOldeMagickShoppe)

  northside <- place Locations.northside_c2026
  _ <- place Locations.easttown_c2026
  _ <- place Locations.merchantDistrict_c2026
  waterfrontDistrict <- place Locations.waterfrontDistrict
  southside <- place Locations.southside_c2026
  frenchHill <- place Locations.frenchHill_c2026

  miskatonicUniversityBurned <- getHasRecord MiskatonicUniversityBurned
  miskatonicUniversitySaved <- getHasRecord InvestigatorsSavedMiskatonicUniversity

  miskatonicUniversity <-
    if miskatonicUniversityBurned
      then place Locations.miskatonicUniversityInFlames
      else place Locations.miskatonicUniversityQuietCampus

  setAgendaDeck [Agendas.arkhamAlive, Agendas.emergentEvils]
  setActDeck [Acts.augursOfFlame]

  when miskatonicUniversityBurned $ removeEvery [Locations.miskatonicUniversityQuietCampus]
  when miskatonicUniversitySaved $ do
    removeEvery [Locations.miskatonicUniversityInFlames]
    placeDoomOnAgenda 1

  n <- getPlayerCount
  placeDoomOnAgenda n

  let peopleOfArkham =
        [ Enemies.davidRenfieldDisillusionedEschatologist
        , Enemies.corneliaAkelyExhaustedSupervisor
        , Enemies.naomiOBannionRunsThisTown
        , Enemies.sgtEarlMonroeDirtyCop
        , Enemies.abigailForemanWaryLibrarian
        , Enemies.margaretLiuBeguilingLoungeSinger
        ]
  peopleCards <- for peopleOfArkham fromGathered1
  (setAsidePerson, remainingPeople) <- sampleWithRest $ fromMaybe (error "impossible") $ nonEmpty peopleCards
  setAside [setAsidePerson]
  recordSetInsert ServantOfElokoss [toCardCode setAsidePerson]

  servantOfFlame <- fromGathered1 Enemies.servantOfFlameOnTheRun
  shuffled <- shuffle (servantOfFlame : remainingPeople)
  for_ (zip [northside, downtown, southside, frenchHill, uptown, waterfrontDistrict] shuffled) \(loc, card) ->
    placeUnderneath loc [card]

  mBearer <- getOwner Assets.drHenryArmitage_c2026
  for_ mBearer \iid -> do
    mcard <- findCardMatch Assets.drHenryArmitage_c2026 <$> field InvestigatorDeck iid
    for_ mcard \card -> putCardIntoPlay iid card

  setAside [Treacheries.markOfElokoss, Treacheries.markOfElokoss, Treacheries.markOfElokoss, Treacheries.markOfElokoss]

  startAt miskatonicUniversity




instance RunMessage SmokeAndMirrors where
  runMessage msg s@(SmokeAndMirrors attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
        p "cultistTokens"
      addChaosToken Cultist
      addChaosToken Cultist
      pure s
    Setup -> runScenarioSetup SmokeAndMirrors attrs $ setupSmokeAndMirrors attrs
    ResolveChaosToken _ Cultist iid -> do
      withSkillTestEnemyTarget \eid -> do
        whenM (eid <=~> EliteEnemy) $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Tablet -> placeCluesOnLocation iid token.face 1
        _ -> pure ()
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      playerCount <- getPlayerCount
      let needTokenCount = playerCount - 1
      let entry x = scope x $ withVars ["perPlayerCount" .= playerCount] $ flavor $ setTitle "title" >> p.green "body"
      let successEntry x = scope x $ scope "success" $ withVars ["perPlayerCount" .= playerCount] $ flavor $ setTitle "title" >> p.green "body"
      case n of
        1 -> do
          entry "davidRenfield"
          eid <- selectJust $ enemyIs Enemies.davidRenfieldDisillusionedEschatologist
          mLoc <- field EnemyLocation eid
          for_ mLoc \loc -> do
            connected <- getConnectedLocations loc
            emptyConnected <- filterM (fmap null . select . investigatorAt) connected
            let targetLocs = case nonEmpty emptyConnected of
                  Just xs -> toList xs
                  Nothing -> connected
            unless (null targetLocs) do
              chooseOneM iid $ targets targetLocs \dest -> push $ EnemyMove eid dest
          placeTokens source (toTarget eid) Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "davidRenfield"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        2 -> do
          entry "corneliaAkely"
          healableInvestigators <- select $ HealableInvestigator (toSource source) #horror Anyone
          healableAssets <- select $ HealableAsset (toSource source) #horror AnyAsset
          chooseOneM iid do
            targets healableInvestigators \target -> healHorror target source 1
            targets healableAssets \target -> healHorror target source 1
          eid <- selectJust $ enemyIs Enemies.corneliaAkelyExhaustedSupervisor
          placeTokens source (toTarget eid) Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "corneliaAkely"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        3 -> do
          entry "naomiOBannion"
          enemies <- select $ EnemyWithoutTrait Humanoid <> EnemyAt Anywhere
          unless (null enemies) do
            chooseOrRunOneM iid do
              labeled "Do not choose an enemy" nothing
              targets enemies \eid' -> defeatEnemy eid' iid source
          eid <- selectJust $ enemyIs Enemies.naomiOBannionRunsThisTown
          placeTokens source (toTarget eid) Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "naomiOBannion"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        4 -> do
          entry "sgtEarlMonroe"
          healableInvestigators <- select $ HealableInvestigator (toSource source) #damage Anyone
          healableAssets <- select $ HealableAsset (toSource source) #damage AnyAsset
          chooseOneM iid do
            targets healableInvestigators \target -> healDamage target source 1
            targets healableAssets \target -> healDamage target source 1
          eid <- selectJust $ enemyIs Enemies.sgtEarlMonroeDirtyCop
          placeTokens source (toTarget eid) Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "sgtEarlMonroe"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        5 -> do
          entry "abigailForeman"
          drawCards iid source 1
          eid <- selectJust $ enemyIs Enemies.abigailForemanWaryLibrarian
          placeTokens source (toTarget eid) Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "abigailForeman"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        6 -> do
          entry "margaretLiu"
          deck <- scenarioField ScenarioEncounterDeck
          for_ (headMay $ unDeck deck) \card -> do
            focusCards [card] $ chooseOneM iid do
              labeled "Discard it" $ push $ DiscardTopOfEncounterDeck iid 1 (toSource source) Nothing
              labeled "Leave it" nothing
          eid <- selectJust $ enemyIs Enemies.margaretLiuBeguilingLoungeSinger
          placeTokens source (toTarget eid) Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "margaretLiu"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        7 -> do
          investigatorStoryWithChooseOneM' iid (scope "servantOfFlame" $ setTitle "title" >> p "body") do
            labeled' "addToVictory" do
              victoryDisplay <- getVictoryDisplay
              let inVictory = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay
              unless inVictory do
                meid <- selectOne $ enemyIs Enemies.servantOfFlameOnTheRun
                for_ meid \eid -> push $ AddToVictory (Just iid) (toTarget eid)
              eachInvestigator \iid' -> drawCards iid' source 3
            labeled' "placeUnderAct" do
              victoryDisplay <- getVictoryDisplay
              mcard <- case find ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay of
                Just c -> pure (Just c)
                Nothing -> do
                  meid <- selectOne $ enemyIs Enemies.servantOfFlameOnTheRun
                  case meid of
                    Just eid -> Just <$> field EnemyCard eid
                    Nothing -> pure Nothing
              case mcard of
                Just card -> do
                  meid <- selectOne $ enemyIs Enemies.servantOfFlameOnTheRun
                  for_ meid \eid -> push $ QuietlyRemoveFromGame (toTarget eid)
                  placeUnderneath ActDeckTarget [card]
                  eachInvestigator \iid' -> gainClues iid' source 1
                Nothing -> error "missing servant of flame"
        _ -> error "invalid codex entry"
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          -- Reveal the harbinger of Elokoss
          servantCodes <- getRecordedCardCodes ServantOfElokoss
          let mHarbingerCode = listToMaybe servantCodes
          let servantName = maybe "unknown" (nameTitle . cdName . lookupEncounterCardDef) mHarbingerCode

          -- Gather Elite enemies in play and/or beneath locations, shuffle with harbinger, and draw
          eliteInPlay <- select $ EliteEnemy <> EnemyAt Anywhere
          eliteInPlayCards <- traverse (field EnemyCard) eliteInPlay

          locationsWithCards <- select $ LocationWithCardsUnderneath AnyCards
          cardsUnderLocations <- concatMapM (field LocationCardsUnderneath) locationsWithCards
          let eliteUnderLocations = filter (\c -> toCardType c == EnemyType && c `cardMatch` CardWithTrait Elite) cardsUnderLocations

          harbingerCard <- for mHarbingerCode \code -> genCard (lookupEncounterCardDef code)

          let allCards = eliteInPlayCards <> eliteUnderLocations <> maybeToList harbingerCard
          mDrawnCard <- case nonEmpty allCards of
            Nothing -> pure Nothing
            Just cards -> do
              shuffled <- shuffle $ toList cards
              case listToMaybe shuffled of
                Just drawnCard | Just harbingerCode <- mHarbingerCode, toCardCode drawnCard == harbingerCode -> do
                  record InvestigatorsDiscoveredTheCultsWhereabouts
                  pure $ Just drawnCard
                Just drawnCard -> do
                  record InvestigatorsFailedInTheirSearch
                  pure $ Just drawnCard
                Nothing -> do
                  record InvestigatorsFailedInTheirSearch
                  pure Nothing

          let drawnCardName = maybe "unknown" (nameTitle . cdName . lookupEncounterCardDef . toCardCode) mDrawnCard

          -- Check if Servant of Flame is in the victory display
          victoryDisplay <- getVictoryDisplay
          let servantInVictory = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay
          when servantInVictory $ record InvestigatorsKilledTheServantOfFlame

          -- Experience: victory X + 1 per enemy beneath the act
          underAct <- scenarioFieldMap ScenarioCardsUnderActDeck id
          let underActEnemyCount = length $ filter ((== EnemyType) . toCardType) underAct
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" underActEnemyCount

          resolutionFlavor $ withVars ["xp" .= xp, "servantName" .= servantName, "drawnCard" .= drawnCardName] $ setTitle "noResolution.title" >> p "noResolution.body"

          -- Check if Servant of Flame was beneath the act
          let servantUnderAct = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) underAct
          when servantUnderAct $ push $ ScenarioResolution $ Resolution 2

        Resolution 1 -> do
          record InvestigatorsDiscoveredTheCultsWhereabouts

          servantCodes <- getRecordedCardCodes ServantOfElokoss
          let mHarbingerCode = listToMaybe servantCodes
          let servantName = maybe "unknown" (nameTitle . cdName . lookupEncounterCardDef) mHarbingerCode

          victoryDisplay <- getVictoryDisplay
          underAct <- scenarioFieldMap ScenarioCardsUnderActDeck id

          let servantInVictory = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay
          when servantInVictory $ record InvestigatorsKilledTheServantOfFlame

          let eliteUnderAct = length $ filter (\c -> toCardType c == EnemyType && c `cardMatch` CardWithTrait Elite) underAct
          when (eliteUnderAct >= 6) $ record InvestigatorsScouredArkhamForAnswers

          let eliteInVictory = length $ filter (\c -> toCardType c == EnemyType && c `cardMatch` CardWithTrait Elite) victoryDisplay
          when (eliteInVictory >= 6) $ record InvestigatorsStirredUpTrouble

          let underActEnemyCount = length $ filter ((== EnemyType) . toCardType) underAct
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" underActEnemyCount

          resolutionFlavor $ withVars ["xp" .= xp, "servantName" .= servantName] $ setTitle "resolution1.title" >> p "resolution1.body"

          let servantUnderAct = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) underAct
          when servantUnderAct $ push $ ScenarioResolution $ Resolution 2

        Resolution 2 -> do
          record TheServantOfFlameEscaped
          eachInvestigator \iid -> gainXp iid attrs (ikey "xp.resolution2") 1

        other -> throwIO $ UnknownResolution other

      markedCodes <- getRecordedCardCodes InvestigatorsWereMarkedByElokoss
      eachInvestigator \iid -> do
        code <- field InvestigatorCardCode iid
        when (code `elem` markedCodes) $ addCampaignCardToDeck iid DoNotShuffleIn Treacheries.markOfElokoss

      endOfScenario
      pure s
    _ -> SmokeAndMirrors <$> liftRunMessage msg attrs
