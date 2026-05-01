module Arkham.Scenario.Scenarios.SmokeAndMirrors (smokeAndMirrors) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.BrethrenOfAsh.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterCard (lookupEncounterCardDef)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Exception
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Campaign (withOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getConnectedLocations, withLocationOf)
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest.Target (withSkillTestEnemyTarget)
import Arkham.Helpers.Xp
import Arkham.I18n (ikey)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  enemyIs,
  investigatorAt,
  pattern AnyAsset,
  pattern AnyCards,
  pattern Anyone,
  pattern Anywhere,
  pattern AssetWithTitle,
  pattern CardWithTrait,
  pattern EliteEnemy,
  pattern EnemyAt,
  pattern EnemyWithoutTrait,
  pattern HealableAsset,
  pattern HealableInvestigator,
  pattern LocationWithCardsUnderneath,
 )
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Modifier (ModifierType (DoNotTakeUpSlot))
import Arkham.Name (nameTitle)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.SmokeAndMirrors.Helpers
import Arkham.Token
import Arkham.Trait (Trait (Elite, Humanoid))
import Arkham.Treachery.Cards qualified as Treacheries

newtype SmokeAndMirrors = SmokeAndMirrors ScenarioAttrs
  deriving stock Generic
  deriving anyclass IsScenario
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

instance RunMessage SmokeAndMirrors where
  runMessage msg s@(SmokeAndMirrors attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      twice $ addChaosToken Cultist

      withOwner Assets.drHenryArmitage_c2026 \iid -> do
        card <- fetchCard Assets.drHenryArmitage_c2026
        obtainCard card
        putCardIntoPlay iid card
      pure s
    Setup -> runScenarioSetup SmokeAndMirrors attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setOutOfPlay"
        li.nested "checkCampaignLog" do
          li "miskatonicUniversityBurned"
          li "savedMiskatonicUniversity"
          li "startAt"
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
      gather Set.Whippoorwills_c2026

      setAgendaDeck [Agendas.arkhamAlive, Agendas.emergentEvils]
      setActDeck [Acts.augursOfFlame]

      downtown <-
        placeLabeled "downtown"
          =<< sampleOneOf (Locations.downtownFirstBankOfArkham_c2026, Locations.downtownArkhamSanatorium)
      uptown <-
        placeLabeled "uptown"
          =<< sampleOneOf (Locations.uptownStMarrysHospital, Locations.uptownYeOldeMagickShoppe)

      place_ Locations.easttown_c2026
      place_ Locations.merchantDistrict_c2026
      northside <- place Locations.northside_c2026
      waterfrontDistrict <- place Locations.waterfrontDistrict
      southside <- place Locations.southside_c2026
      frenchHill <- place Locations.frenchHill_c2026

      (setAsidePerson, remainingPeople) <-
        sampleWithRest
          <=< traverse fromGathered1
          $ Enemies.davidRenfieldDisillusionedEschatologist
          :| [ Enemies.corneliaAkelyExhaustedSupervisor
             , Enemies.naomiOBannionRunsThisTown
             , Enemies.sgtEarlMonroeDirtyCop
             , Enemies.abigailForemanWaryLibrarian
             , Enemies.margaretLiuBeguilingLoungeSinger
             ]
      setAsideFacedown [setAsidePerson]
      setMeta (toCardDef setAsidePerson)
      -- recordSetInsert ServantOfElokoss [toCardCode setAsidePerson]

      servantOfFlame <- fromGathered1 Enemies.servantOfFlameOnTheRun
      shuffled <- shuffle $ servantOfFlame : remainingPeople
      for_ (zip [northside, downtown, southside, frenchHill, uptown, waterfrontDistrict] shuffled) \(loc, card) ->
        placeUnderneath loc [card]

      miskatonicUniversityBurned <- getHasRecord MiskatonicUniversityBurned
      miskatonicUniversitySaved <- getHasRecord InvestigatorsSavedMiskatonicUniversity

      startAt
        =<< if miskatonicUniversityBurned
          then place Locations.miskatonicUniversityInFlames
          else place Locations.miskatonicUniversityQuietCampus

      when miskatonicUniversityBurned $ removeEvery [Locations.miskatonicUniversityQuietCampus]
      when miskatonicUniversitySaved do
        removeEvery [Locations.miskatonicUniversityInFlames]
        placeDoomOnAgenda 1

      placeDoomOnAgenda =<< perPlayer 1

      setAside
        [ Treacheries.markOfElokoss
        , Treacheries.markOfElokoss
        , Treacheries.markOfElokoss
        , Treacheries.markOfElokoss
        ]
    ResolveChaosToken _ Cultist iid -> do
      withSkillTestEnemyTarget \eid -> do
        whenM (eid <=~> EliteEnemy) $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == Tablet) do
        placeCluesOnLocation iid token.face 1
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      playerCount <- getPlayerCount
      let needTokenCount = playerCount - 1
      let entry x =
            scope x
              $ withVars ["perPlayerCount" .= playerCount]
              $ flavor
              $ setTitle "title"
              >> compose.codex (h "title" >> p "body")
      let successEntry x =
            scope x
              $ withVars ["perPlayerCount" .= playerCount]
              $ flavor
              $ setTitle "title"
              >> compose.codex (h "title" >> p "success")
      case n of
        1 -> do
          entry "davidRenfield"
          eid <- selectJust $ enemyIs Enemies.davidRenfieldDisillusionedEschatologist
          withLocationOf eid \loc -> do
            connected <- getConnectedLocations loc
            emptyConnected <- filterM (fmap null . select . investigatorAt) connected
            let targetLocs = maybe connected toList (nonEmpty emptyConnected)
            chooseOneM iid $ targets targetLocs $ push . EnemyMove eid
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
            targets healableInvestigators $ healHorrorOn source 1
            targets healableAssets $ healHorrorOn source 1
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
          placeTokens source eid Resource 1
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
            targets healableInvestigators $ healDamageOn source 1
            targets healableAssets $ healDamageOn source 1
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
        Phi -> do
          investigatorStoryWithChooseOneM'
            iid
            (scope "servantOfFlame" $ setTitle "title" >> compose.codex (h "title" >> p "body"))
            do
              labeled' "addToVictory" do
                victoryDisplay <- getVictoryDisplay
                let inVictory = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay
                unless inVictory do
                  meid <- selectOne $ enemyIs Enemies.servantOfFlameOnTheRun
                  for_ meid $ addToVictory iid
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
                    for_ meid $ push . QuietlyRemoveFromGame . toTarget
                    placeUnderneath ActDeckTarget [card]
                    eachInvestigator \iid' -> gainClues iid' source 1
                  Nothing -> error "missing servant of flame"
        _ -> error "invalid codex entry"
      pure s
    ScenarioResolution r -> scope "resolutions" do
      let servantDef = toResult @CardDef attrs.meta
      let servantName = nameTitle $ cdName servantDef
      case r of
        NoResolution -> do
          -- Gather Elite enemies in play and/or beneath locations, shuffle with harbinger, and draw
          eliteInPlay <- select $ EliteEnemy <> EnemyAt Anywhere
          eliteInPlayCards <- traverse (field EnemyCard) eliteInPlay

          locationsWithCards <- select $ LocationWithCardsUnderneath AnyCards
          cardsUnderLocations <- concatMapM (field LocationCardsUnderneath) locationsWithCards
          let eliteUnderLocations = filter (\c -> toCardType c == EnemyType && c `cardMatch` CardWithTrait Elite) cardsUnderLocations

          harbingerCard <- fetchCard servantDef

          let allCards = eliteInPlayCards <> eliteUnderLocations <> [harbingerCard]
          mDrawnCard <- case nonEmpty allCards of
            Nothing -> pure Nothing
            Just cards -> do
              shuffled <- shuffle $ toList cards
              case listToMaybe shuffled of
                Just drawnCard | toCardCode drawnCard == toCardCode servantDef -> do
                  record InvestigatorsDiscoveredTheCultsWhereabouts
                  pure $ Just drawnCard
                Just drawnCard -> do
                  record InvestigatorsFailedInTheirSearch
                  pure $ Just drawnCard
                Nothing -> do
                  record InvestigatorsFailedInTheirSearch
                  pure Nothing

          let drawnCardName = maybe "unknown" (nameTitle . cdName . lookupEncounterCardDef . toCardCode) mDrawnCard

          victoryDisplay <- getVictoryDisplay
          let servantInVictory = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay
          when servantInVictory $ record InvestigatorsKilledTheServantOfFlame

          underAct <- scenarioFieldMap ScenarioCardsUnderActDeck id
          let underActEnemyCount = length $ filter ((== EnemyType) . toCardType) underAct
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" underActEnemyCount

          resolutionFlavor
            $ withVars ["xp" .= xp, "servantName" .= servantName, "drawnCard" .= drawnCardName]
            $ setTitle "noResolution.title"
            >> p "noResolution.body"

          let servantUnderAct = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) underAct
          if servantUnderAct then push R2 else endOfScenario
        Resolution 1 -> do
          record InvestigatorsDiscoveredTheCultsWhereabouts

          victoryDisplay <- getVictoryDisplay
          underAct <- scenarioFieldMap ScenarioCardsUnderActDeck id

          let servantInVictory = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay
          when servantInVictory $ record InvestigatorsKilledTheServantOfFlame

          let eliteUnderAct = count (\c -> toCardType c == EnemyType && c `cardMatch` CardWithTrait Elite) underAct
          when (eliteUnderAct >= 6) $ record InvestigatorsScouredArkhamForAnswers

          let eliteInVictory =
                length
                  $ filter (\c -> toCardType c == EnemyType && c `cardMatch` CardWithTrait Elite) victoryDisplay
          when (eliteInVictory >= 6) $ record InvestigatorsStirredUpTrouble

          let underActEnemyCount = length $ filter ((== EnemyType) . toCardType) underAct
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" underActEnemyCount

          resolutionFlavor
            $ withVars ["xp" .= xp, "servantName" .= servantName]
            $ setTitle "resolution1.title"
            >> p "resolution1.body"

          let servantUnderAct = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) underAct
          if servantUnderAct then push R2 else endOfScenario
        Resolution 2 -> do
          record TheServantOfFlameEscaped
          eachInvestigator \iid -> gainXp iid attrs (ikey "xp.resolution2") 1
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> SmokeAndMirrors <$> liftRunMessage msg attrs
