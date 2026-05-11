module Arkham.Scenario.Scenarios.SmokeAndMirrors (smokeAndMirrors) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.BrethrenOfAsh.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
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
import Arkham.I18n (cardNameVar, ikey)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (
  EnemyMatcher (InPlayEnemy),
  cardIs,
  enemyIs,
  investigatorAt,
  pattern AnyCards,
  pattern Anywhere,
  pattern AssetWithTitle,
  pattern CardWithTrait,
  pattern EliteEnemy,
  pattern EnemyAt,
  pattern EnemyWithoutTrait,
  pattern LocationWithCardsUnderneath,
 )
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record, recordSetInsert)
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.Modifier (ModifierType (DoNotTakeUpSlot))
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
    [ "northside            downtown         easttown"
    , "miskatonicUniversity merchantDistrict waterfrontDistrict"
    , "uptown               southside        frenchHill"
    ]

instance HasChaosTokenValue SmokeAndMirrors where
  getChaosTokenValue iid chaosTokenFace (SmokeAndMirrors attrs) = case chaosTokenFace of
    Skull -> do
      victoryDisplay <- scenarioFieldMap ScenarioVictoryDisplay (filter ((== EnemyType) . toCardType))
      underAct <- scenarioFieldMap ScenarioCardsUnderActDeck (filter ((== EnemyType) . toCardType))
      let eliteCount = count (\c -> cardMatch c (CardWithTrait Elite)) (victoryDisplay <> underAct)
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

      withOwner Assets.drHenryArmitage_SpreadingFlames \iid -> do
        card <- fetchCard Assets.drHenryArmitage_SpreadingFlames
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
      gather Set.Arkham
      gather Set.BadWeather
      gather Set.DeadEnds
      gather Set.FlyingTerrors
      gather Set.GangsOfArkham
      gather Set.PeopleOfArkham
      gather Set.Whippoorwills2

      setAgendaDeck [Agendas.arkhamAlive, Agendas.emergentEvils]
      setActDeck [Acts.augursOfFlame]

      downtown <-
        placeLabeled "downtown"
          =<< sampleOneOf (Locations.downtownFirstBankOfArkham_Arkham, Locations.downtownArkhamSanatorium)
      uptown <-
        placeLabeled "uptown"
          =<< sampleOneOf (Locations.uptownStMarysHospital, Locations.uptownYeOldeMagickShoppe)

      place_ Locations.easttown_Arkham
      place_ Locations.merchantDistrict_Arkham
      northside <- place Locations.northside_Arkham
      waterfrontDistrict <- place Locations.waterfrontDistrict
      southside <- place Locations.southside_Arkham
      frenchHill <- place Locations.frenchHill_Arkham

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
      let entry x = scope x $ flavor $ setTitle "title" >> compose.codex (h "title" >> p "body")
      let successEntry x = scope x $ flavor $ setTitle "title" >> compose.codex (h "title" >> p "success")
      case n of
        1 -> do
          entry "davidRenfield"
          eid <- selectJust $ enemyIs Enemies.davidRenfieldDisillusionedEschatologist
          withLocationOf eid \loc -> do
            connected <- getConnectedLocations loc
            emptyConnected <- filterM (selectNone . investigatorAt) connected
            let targetLocs = maybe connected toList (nonEmpty emptyConnected)
            chooseOneM iid $ targets targetLocs $ enemyMoveTo ScenarioSource eid
          placeTokens source eid Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "davidRenfield"
            removeEnemy eid
            card <- field EnemyCard eid
            placeUnderneath ActDeckTarget [card]
        2 -> do
          entry "corneliaAkely"
          healHorrorIfCan iid ScenarioSource 1
          eid <- selectJust $ enemyIs Enemies.corneliaAkelyExhaustedSupervisor
          placeTokens source eid Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "corneliaAkely"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        3 -> do
          entry "naomiOBannion"
          enemies <- select $ EnemyWithoutTrait Humanoid <> EnemyAt Anywhere
          chooseOrRunOneM iid do
            targets enemies \eid' -> defeatEnemy eid' iid source
            unscoped skip_
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
          healDamageIfCan iid ScenarioSource 1
          eid <- selectJust $ enemyIs Enemies.sgtEarlMonroeDirtyCop
          placeTokens source eid Resource 1
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
          placeTokens source eid Resource 1
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
              unscoped $ labeled' "discard" $ discardTopOfEncounterDeck iid source 1
              unscoped skip_
          eid <- selectJust $ enemyIs Enemies.margaretLiuBeguilingLoungeSinger
          placeTokens source eid Resource 1
          currentTokens <- fieldMap EnemyTokens (countTokens Resource) eid
          when (currentTokens >= needTokenCount) do
            successEntry "margaretLiu"
            card <- field EnemyCard eid
            removeEnemy eid
            placeUnderneath ActDeckTarget [card]
        Phi -> do
          victoryDisplay <- getVictoryDisplay
          investigatorStoryWithChooseOneM'
            iid
            (scope "servantOfFlame" $ setTitle "title" >> compose.codex (h "title" >> p "body"))
            do
              labeled' "addToVictory" do
                let inVictory = any ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay
                unless inVictory do
                  selectEach (enemyIs Enemies.servantOfFlameOnTheRun) (addToVictory iid)
                eachInvestigator \iid' -> drawCards iid' source 3
              labeled' "placeUnderAct" do
                mcard <- case find ((== toCardCode Enemies.servantOfFlameOnTheRun) . toCardCode) victoryDisplay of
                  Just c -> pure (Just c)
                  Nothing -> runMaybeT do
                    eid <- MaybeT $ selectOne $ enemyIs Enemies.servantOfFlameOnTheRun
                    lift $ field EnemyCard eid
                case mcard of
                  Just card -> do
                    selectEach (enemyIs Enemies.servantOfFlameOnTheRun) (push . QuietlyRemoveFromGame . toTarget)
                    placeUnderneath ActDeckTarget [card]
                    eachInvestigator \iid' -> gainClues iid' source 1
                  Nothing -> error "missing servant of flame"
        _ -> error "invalid codex entry"
      pure s
    ScenarioResolution r -> scope "resolutions" do
      let servantDef = toResult @CardDef attrs.meta
      case r of
        NoResolution -> do
          recordSetInsert ServantOfElokoss [servantDef.cardCode]
          eliteInPlay <- selectField EnemyCard $ InPlayEnemy $ EliteEnemy <> EnemyAt Anywhere

          cardsUnderLocations <-
            concatMapM (field LocationCardsUnderneath) =<< select (LocationWithCardsUnderneath AnyCards)
          let eliteUnderLocations = filterCards (#enemy <> CardWithTrait Elite) cardsUnderLocations

          harbinger <- fetchCard servantDef
          drawnCard <- sample $ harbinger :| eliteInPlay <> eliteUnderLocations

          let isHarbinger = drawnCard.cardCode == servantDef.cardCode

          record
            $ if isHarbinger
              then InvestigatorsDiscoveredTheCultsWhereabouts
              else InvestigatorsFailedInTheirSearch

          whenM (inVictoryDisplay $ cardIs Enemies.servantOfFlameOnTheRun) do
            record InvestigatorsKilledTheServantOfFlame

          underAct <- scenarioField ScenarioCardsUnderActDeck
          let underActEnemyCount = countCards (card_ #enemy) underAct

          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" underActEnemyCount
          resolutionFlavor $ scope "noResolution" do
            setTitle "title"
            p "body"
            ul do
              cardNameVar servantDef $ li "harbingerOfElokoss"
              cardNameVar drawnCard $ li.nested "drawnCard" do
                li.validate isHarbinger "isHarbinger"
                li.validate (not isHarbinger) "otherwise"
              li "servantOfFlame"
              withVars ["xp" .= xp] $ li "victory"
              li "proceed"

          let servantUnderAct = any (isCardCode Enemies.servantOfFlameOnTheRun) underAct
          if servantUnderAct then push R2 else endOfScenario
        Resolution 1 -> do
          recordSetInsert ServantOfElokoss [toCardCode servantDef]
          record InvestigatorsDiscoveredTheCultsWhereabouts

          whenM (inVictoryDisplay $ cardIs Enemies.servantOfFlameOnTheRun) do
            record InvestigatorsKilledTheServantOfFlame

          underAct <- scenarioField ScenarioCardsUnderActDeck

          let eliteUnderAct = countCards (#enemy <> CardWithTrait Elite) underAct
          when (eliteUnderAct >= 6) $ record InvestigatorsScouredArkhamForAnswers

          eliteInVictory <- countCards (#enemy <> CardWithTrait Elite) <$> getVictoryDisplay
          when (eliteInVictory >= 6) $ record InvestigatorsStirredUpTrouble

          let underActEnemyCount = countCards (card_ #enemy) underAct

          cardNameVar servantDef
            $ resolutionWithXp "resolution1"
            $ allGainXpWithBonus' attrs
            $ toBonus "bonus" underActEnemyCount

          let servantUnderAct = any (isCardCode Enemies.servantOfFlameOnTheRun) underAct
          if servantUnderAct then push R2 else endOfScenario
        Resolution 2 -> do
          record TheServantOfFlameEscaped
          eachInvestigator \iid -> gainXp iid attrs (ikey "xp.resolution2") 1
          resolution "resolution2"
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> SmokeAndMirrors <$> liftRunMessage msg attrs
