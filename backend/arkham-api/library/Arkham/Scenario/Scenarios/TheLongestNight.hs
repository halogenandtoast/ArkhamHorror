module Arkham.Scenario.Scenarios.TheLongestNight (theLongestNight) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectWith)
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Helpers.Window (wouldDo)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Modifier (setActiveDuringSetup)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.SortedPair
import Arkham.Story.Cards qualified as Stories
import Arkham.Token
import Arkham.Trait (Trait (Madness))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window
import Control.Lens (non)
import Data.Map.Strict qualified as Map

newtype TheLongestNight = TheLongestNight ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheLongestNight where
  getModifiersFor (TheLongestNight a) = do
    modifySelectWith
      a
      (assetIs Assets.drRosaMarquezBestInHerField)
      setActiveDuringSetup
      [DoNotTakeUpSlot #ally]
    modifySelectWith
      a
      (assetIs Assets.helenPetersTheEldestSister)
      setActiveDuringSetup
      [DoNotTakeUpSlot #ally]
    modifySelectWith a (assetIs Assets.ajax) setActiveDuringSetup [DoNotTakeUpSlot #ally]
    modifySelect a Anyone [CanIgnoreBarriers]

theLongestNight :: Difficulty -> TheLongestNight
theLongestNight difficulty = scenario TheLongestNight "10626" "The Longest Night" difficulty []

instance HasChaosTokenValue TheLongestNight where
  getChaosTokenValue iid tokenFace (TheLongestNight attrs) = case tokenFace of
    Skull -> do
      doom <- getDoomCount
      pure $ toChaosTokenValue attrs Skull ((doom + 1) `div` 2) doom
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheLongestNight where
  runMessage msg s@(TheLongestNight attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (h "title" >> p "intro1") do
        labeled' "confront" $ doStep 2 PreScenarioSetup
        labeled' "keepHidden" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      addChaosToken #tablet
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      addChaosToken #skull
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      danceCount <-
        length
          <$> filterM
            getHasRecord
            [ toCampaignLogKey SimeonSharedADance
            , toCampaignLogKey LeahSharedADance
            , toCampaignLogKey TheoSharedADance
            , toCampaignLogKey GideonSharedADance
            , toCampaignLogKey JudithSharedADance
            , toCampaignLogKey WilliamSharedADance
            , toCampaignLogKey RiverSharedADance
            , toCampaignLogKey HelenSharedADance
            ]
      flavor do
        setTitle "title"
        p "intro4"
        p.basic "checkDance"
        ul do
          li.validate (danceCount >= 2) "danceTwo"
          li.validate (danceCount == 1) "danceOne"
          li.validate (danceCount == 0) "danceNone"
      if danceCount >= 2
        then doStep 5 PreScenarioSetup
        else
          if danceCount == 1
            then doStep 6 PreScenarioSetup
            else doStep 7 PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro5"
      addChaosToken #cultist
      doStep 8 PreScenarioSetup
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro6"
      addChaosToken #cultist
      addChaosToken #elderthing
      doStep 8 PreScenarioSetup
      pure s
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro7"
      record TheInvestigatorsFacedTheLongestNightAlone
      addChaosToken #elderthing
      doStep 8 PreScenarioSetup
      pure s
    DoStep 8 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro8"
      pure s
    Setup -> runScenarioSetup TheLongestNight attrs do
      facedAlone <- getHasRecord TheInvestigatorsFacedTheLongestNightAlone
      helenDanced <- getHasRecord HelenSharedADance
      bearWounded <- getHasRecord TheBearWasWounded

      setup $ ul do
        li "gatherSets"
        li "midnightMasks"
        li "nightTwo"
        li "removeOuterFields"
        li.nested "locations" do
          li "placeLocations"
          li "theCaptives"
          li "startingLocation"
        li.nested "enemyDeck" do
          li "gatherEnemies"
          li "ursineHybrid"
          li "shuffleEnemyDeck"
        li.validate bearWounded "bearWounded"
        li.nested "defenses" do
          li "placeDefenses"
          li "tokenReferenceCard"
        li.nested "clues" do
          li.validate facedAlone "facedAlone"
          li.validate (not facedAlone) "facedWithHelp"
        li.nested "residents" do
          li "drMarquez"
          li "dancedResidents"
          li.validate helenDanced "helenPeters"
          li "removeResidents"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      scope "barriers" $ flavor $ setTitle "title" >> p "body"
      scope "decoys" $ flavor $ setTitle "title" >> p "body"
      scope "traps" $ flavor $ setTitle "title" >> p "body"
      scope "enemyDeckRules" $ flavor $ setTitle "title" >> p "body"

      setUsesGrid

      gather Set.TheSecondDay
      gather Set.TheLongestNight
      gather Set.Blight
      gather Set.Transfiguration
      gather Set.Fire
      gather Set.ChillingCold
      gatherJust Set.TheMidnightMasks [Treacheries.huntingShadow, Treacheries.falseLead]
      gather Set.StrikingFear

      placeStory Stories.nightTwo
      placeStory Stories.barriersDecoysAndTraps
      setScenarioDayAndTime

      setAgendaDeck [Agendas.theOnslaught]
      setActDeck [Acts.theLongestNight]

      (removed, remainingFields) <-
        splitAt 1
          <$> shuffle
            [ Locations.outerFieldsBloodiedPaths
            , Locations.outerFieldsDesolateHills
            , Locations.outerFieldsBlightedCornfields
            , Locations.outerFieldsScorchedKnoll
            , Locations.outerFieldsRancidCrops
            ]

      removeEvery removed

      farmhouse <- placeInGrid (Pos 0 0) Locations.theFarmhouse
      atwoodFarms <-
        shuffle [Locations.milkhouse, Locations.vineyard, Locations.coop, Locations.barn, Locations.pasture]
      let farmPositions = [Pos (-1) 0, Pos (-2) 0, Pos 1 0, Pos 0 (-1), Pos 0 1]
      farmLids <- for (zip farmPositions atwoodFarms) (uncurry placeInGrid)

      let westOuterPos = Pos (-3) 0
      let outerPositions = [westOuterPos, Pos 2 0, Pos 0 (-2), Pos 0 2]
      outerFields <- for (zip outerPositions remainingFields) \(pos, card) ->
        (pos,) <$> placeInGrid pos card
      let outerLids = map snd outerFields
      let allLocations = farmhouse : farmLids <> outerLids

      assetAt_ Assets.theCaptives farmhouse

      eachInvestigator \iid -> chooseTargetM iid allLocations $ moveTo_ attrs iid

      let westOuterFieldsLid = maybe farmhouse snd $ find ((== westOuterPos) . fst) outerFields
      ursine <- enemyAt Enemies.ursineHybridStarvingAbomination westOuterFieldsLid

      enemyCards <- fromGathered (CardFromEncounterSet Set.TheLongestNight <> #enemy)
      addExtraDeck EnemyDeck =<< shuffle enemyCards

      when bearWounded do
        nonAttackEnemyDamage Nothing ScenarioSource 2 ursine
        exhaustEnemy ScenarioSource ursine

      doStep 1 msg

      let clueCount = if facedAlone then 3 else 2
      eachInvestigator \iid -> gainClues iid ScenarioSource clueCount

      investigators <- allInvestigators

      drMarquez <- createAsset =<< genCard Assets.drRosaMarquezBestInHerField
      leadChooseOneM do
        unscoped
          $ nameVar Assets.drRosaMarquezBestInHerField
          $ questionLabeled' "chooseInvestigatorToTakeControlOf"
        questionLabeledCard Assets.drRosaMarquezBestInHerField
        portraits investigators (`takeControlOfAsset` drMarquez)

      let danceChecks =
            [ (toCampaignLogKey SimeonSharedADance, SimeonAtwood)
            , (toCampaignLogKey LeahSharedADance, LeahAtwood)
            , (toCampaignLogKey TheoSharedADance, TheoPeters)
            , (toCampaignLogKey GideonSharedADance, GideonMizrah)
            , (toCampaignLogKey JudithSharedADance, JudithPark)
            , (toCampaignLogKey WilliamSharedADance, WilliamHemlock)
            , (toCampaignLogKey RiverSharedADance, RiverHawthorne)
            ]
      dancedResidents <-
        catMaybes <$> for danceChecks \(k, resident) -> do
          danced <- getHasRecord k
          pure $ if danced then Just resident else Nothing

      for_ dancedResidents \resident -> do
        residentAsset <- createAsset =<< genCard (toCardDef resident)
        leadChooseOneM do
          unscoped
            $ nameVar (toCardDef resident)
            $ questionLabeled' "chooseInvestigatorToTakeControlOf"
          questionLabeledCard (toCardDef resident)
          portraits investigators (`takeControlOfAsset` residentAsset)

      when helenDanced do
        helenPeters <- createAsset =<< genCard Assets.helenPetersTheEldestSister
        leadChooseOneM do
          unscoped
            $ nameVar Assets.helenPetersTheEldestSister
            $ questionLabeled' "chooseInvestigatorToTakeControlOf"
          questionLabeledCard Assets.helenPetersTheEldestSister
          portraits investigators (`takeControlOfAsset` helenPeters)

      setAsideEvery (cardIs Treacheries.fire)
      setAside [Assets.ajax]
    DoStep 1 Setup -> do
      lead <- getLead
      allLocations <- select Anywhere
      decoyLocations <- filterM (\lid -> lid <=~> LocationWithoutModifier CannotHaveDecoys) allLocations
      chooseOneM lead do
        questionLabeled' "placeDecoy"
        unterminated $ for_ decoyLocations \lid -> targeting lid $ placeDecoy ScenarioSource lid
      trapLocations <- filterM (\lid -> lid <=~> LocationWithoutModifier CannotHaveTraps) allLocations
      chooseOneM lead do
        questionLabeled' "placeTrap"
        unterminated $ for_ trapLocations \lid -> targeting lid $ placeTrap ScenarioSource lid
      chooseOneM lead do
        questionLabeled' "placeBarrier"
        unterminated $ for_ allLocations \lid -> targeting lid $ forTarget lid Setup
      pure s
    ForTarget (LocationTarget lid) Setup -> do
      lead <- getLead
      connected <- select $ connectedTo (LocationWithId lid)
      chooseTargetM lead connected \lid2 ->
        push $ ScenarioCountIncrementBy (Barriers lid lid2) 1
      pure s
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      withLocationOf iid \lid -> doStep 1 (ForTarget (LocationTarget lid) (ScenarioSpecific "placeBarrierAt" (toJSON iid)))
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isHardExpert attrs -> do
          withLocationOf iid \lid -> doStep 1 (ForTarget (LocationTarget lid) (ScenarioSpecific "placeBarrierAt" (toJSON iid)))
        _ -> pure ()
      pure s
    DoStep 1 (ForTarget (LocationTarget lid) (ScenarioSpecific "placeBarrierAt" v)) -> do
      let iid :: InvestigatorId = toResult v
      connected <- select $ connectedTo (LocationWithId lid)
      chooseTargetM iid connected \lid2 ->
        push $ ScenarioCountIncrementBy (Barriers lid lid2) 1
      pure s
    ResolveChaosToken _ Tablet iid | isEasyStandard attrs -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      doStep 1 (ScenarioSpecific "nearestEnemyAttacks" (toJSON iid))
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      withLocationOf iid \lid -> doStep 1 (ForTarget (LocationTarget lid) (ScenarioSpecific "removeDefenseAt" (toJSON iid)))
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Tablet | isEasyStandard attrs -> doStep 1 (ScenarioSpecific "nearestEnemyAttacks" (toJSON iid))
        ElderThing | isEasyStandard attrs ->
          withLocationOf iid \lid -> doStep 1 (ForTarget (LocationTarget lid) (ScenarioSpecific "removeDefenseAt" (toJSON iid)))
        _ -> pure ()
      pure s
    DoStep 1 (ScenarioSpecific "nearestEnemyAttacks" v) -> do
      let iid :: InvestigatorId = toResult v
      enemies <- select $ NearestEnemyTo iid AnyEnemy
      chooseTargetM iid enemies \eid -> do
        ready eid
        resolveHunterKeyword eid
        doStep 2 (ForTarget (EnemyTarget eid) (ScenarioSpecific "nearestEnemyAttacks" v))
      pure s
    DoStep 2 (ForTarget (EnemyTarget eid) (ScenarioSpecific "nearestEnemyAttacks" v)) -> do
      let iid :: InvestigatorId = toResult v
      whenMatch eid (enemyEngagedWith iid) $ initiateEnemyAttack eid ScenarioSource iid
      pure s
    DoStep 1 (ForTarget (LocationTarget lid) (ScenarioSpecific "removeDefenseAt" v)) -> do
      let iid :: InvestigatorId = toResult v
      hasDecoy <- lid <=~> LocationWithHorror (atLeast 1)
      hasTrap <- lid <=~> LocationWithDamage (atLeast 1)
      connected <- select $ connectedTo (LocationWithId lid)
      let meta = toResultDefault defaultMeta attrs.meta
      let barrierPairs =
            filter
              (\lid2 -> Map.findWithDefault 0 (sortedPair lid lid2) meta.barriers > 0)
              connected
      when (hasDecoy || hasTrap || notNull barrierPairs) do
        chooseOneM iid do
          when hasDecoy do
            labeled' "removeDecoy" $ removeTokens ScenarioSource lid Horror 1
          when hasTrap do
            labeled' "removeTrap" $ removeTokens ScenarioSource lid Damage 1
          when (notNull barrierPairs) do
            labeled' "removeBarrier" do
              chooseTargetM iid barrierPairs \lid2 ->
                push $ ScenarioCountDecrementBy (Barriers lid lid2) 1
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      case n of
        2 -> do
          codexFinished 2
          entry "leahAtwood"
          record LeahStoodByYou
          enemies <- select $ NearestEnemyTo iid AnyEnemy
          chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) source 3
          pure s
        3 -> do
          codexFinished 3
          entry "simeonAtwood"
          record SimeonStoodByYou
          decoyLocations <- select $ LocationWithoutModifier CannotHaveDecoys
          chooseTargetM iid decoyLocations \lid -> placeDecoy source lid
          pure s
        4 -> do
          codexFinished 4
          entry "williamHemlock"
          record WilliamStoodByYou
          eachInvestigator \iid' -> gainClues iid' source 1
          pure s
        5 -> do
          codexFinished 5
          entry "riverHawthorne"
          record RiverStoodByYou
          enemies <- select $ NonEliteEnemy
          allLocations <- select Anywhere
          chooseTargetM iid enemies \eid ->
            chooseTargetM iid allLocations $ enemyMoveTo source eid
          pure s
        6 -> do
          codexFinished 6
          entry "gideonMizrah"
          record GideonStoodByYou
          let meta = toResultDefault defaultMeta attrs.meta
          pure $ TheLongestNight $ attrs & metaL .~ toJSON meta {discardNextEnemyDraw = True}
        7 -> do
          codexFinished 7
          entry "judithPark"
          record JudithStoodByYou
          trapLocations <- select $ LocationWithoutModifier CannotHaveTraps
          chooseTargetM iid trapLocations $ placeTrap source
          pure s
        8 -> do
          codexFinished 8
          entry "theoPeters"
          record TheoStoodByYou
          doStep 1 msg
          pure s
        Theta -> do
          codexFinished Theta
          entry "drRosaMarquez"
          lid <- selectJust $ locationWithInvestigator iid
          doStep 2 (ForTarget (LocationTarget lid) msg)
          doStep 2 (ForTarget (LocationTarget lid) msg)
          pure s
        Omega -> do
          codexFinished Omega
          entry "ajax"
          ajax <- selectJust $ assetIs Assets.ajax
          takeControlOfAsset iid ajax
          pure s
        _ -> pure s
    DoStep 1 (ScenarioSpecific "codex" v) -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, _n :: Int) = toResult v
      lid <- selectJust $ locationWithInvestigator iid
      hasDecoy <- lid <=~> LocationWithHorror (atLeast 1)
      hasTrap <- lid <=~> LocationWithDamage (atLeast 1)
      decoyDestinations <- select $ LocationWithoutModifier CannotHaveDecoys <> not_ (LocationWithHorror (atLeast 1))
      trapDestinations <- select $ LocationWithoutModifier CannotHaveTraps <> not_ (LocationWithDamage (atLeast 1))
      let canMoveDecoy = hasDecoy && notNull decoyDestinations
      let canMoveTrap = hasTrap && notNull trapDestinations
      connected <- select $ connectedTo (LocationWithId lid)
      let meta = toResultDefault defaultMeta attrs.meta
      let barrierPairs =
            filter
              (\lid2 -> Map.findWithDefault 0 (sortedPair lid lid2) meta.barriers > 0)
              connected
      let hasOptions = canMoveDecoy || canMoveTrap || notNull barrierPairs
      allLocations <- select Anywhere
      when hasOptions do
        chooseOneM iid do
          when canMoveDecoy do
            labeled' "moveDecoy" do
              chooseTargetM iid decoyDestinations \toLid -> do
                removeTokens source lid Horror 1
                placeTokens source toLid Horror 1
                selectEach (enemyEngagedWith iid) $ disengageEnemy iid
                moveTo_ source iid toLid
          when canMoveTrap do
            labeled' "moveTrap" do
              chooseTargetM iid trapDestinations \toLid -> do
                removeTokens source lid Damage 1
                placeTokens source toLid Damage 1
                selectEach (enemyEngagedWith iid) $ disengageEnemy iid
                moveTo_ source iid toLid
          when (notNull barrierPairs) do
            labeled' "moveBarrier" do
              chooseTargetM iid barrierPairs \fromLid -> do
                push $ ScenarioCountDecrementBy (Barriers lid fromLid) 1
                chooseTargetM iid allLocations \toLid -> do
                  connectedToTarget <- select $ connectedTo (LocationWithId toLid)
                  chooseTargetM iid connectedToTarget \toLid2 ->
                    push $ ScenarioCountIncrementBy (Barriers toLid toLid2) 1
                  selectEach (enemyEngagedWith iid) $ disengageEnemy iid
                  moveTo_ source iid toLid
      pure s
    DoStep 2 (ForTarget (LocationTarget lid) (ScenarioSpecific "codex" v)) -> do
      let (iid :: InvestigatorId, _source :: Source, _n :: Int) = toResult v
      connected <- select $ connectedTo (LocationWithId lid)
      chooseTargetM iid connected \toLid ->
        push $ ScenarioCountIncrementBy (Barriers lid toLid) 1
      pure s
    DrewCards _iid drew
      | drew.deck == Deck.ScenarioDeckByKey EnemyDeck -> do
          let meta = toResultDefault defaultMeta attrs.meta
          if meta.discardNextEnemyDraw
            then do
              pure
                $ TheLongestNight
                $ attrs
                & metaL
                .~ toJSON meta {discardNextEnemyDraw = False}
                & deckDiscardsL
                . at EnemyDeck
                . non []
                %~ (drew.cards <>)
            else TheLongestNight <$> liftRunMessage msg attrs
    ScenarioSpecific "placeTrap" v -> do
      let (_source :: Source, lid :: LocationId) = toResult v
      wouldDo
        msg
        (Window.ScenarioEvent ("wouldPlaceTrap:" <> tshow lid) Nothing v)
        (Window.ScenarioEvent "placedTrap" Nothing v)
      pure s
    DoBatch _ (ScenarioSpecific "placeTrap" v) -> do
      let (source :: Source, lid :: LocationId) = toResult v
      placeTokens source lid Damage 1
      pure s
    ScenarioSpecific "placeDecoy" v -> do
      let (_source :: Source, lid :: LocationId) = toResult v
      wouldDo
        msg
        (Window.ScenarioEvent ("wouldPlaceDecoy:" <> tshow lid) Nothing v)
        (Window.ScenarioEvent "placedDecoy" Nothing v)
      pure s
    DoBatch _ (ScenarioSpecific "placeDecoy" v) -> do
      let (source :: Source, lid :: LocationId) = toResult v
      placeTokens source lid Horror 1
      pure s
    ScenarioSpecific "discardFromEnemyDeck" v -> do
      let cards :: [Card] = toResult v
      pure $ TheLongestNight $ attrs & deckDiscardsL . at EnemyDeck . non [] %~ (cards <>)
    ScenarioCountIncrementBy (Barriers l1 l2) n -> do
      let meta' = incrementBarriers n l1 l2 $ toResultDefault defaultMeta attrs.meta
      pure $ TheLongestNight $ attrs & metaL .~ toJSON meta'
    ScenarioCountDecrementBy (Barriers l1 l2) n -> do
      let meta' = decrementBarriers n l1 l2 $ toResultDefault defaultMeta attrs.meta
      pure $ TheLongestNight $ attrs & metaL .~ toJSON meta'
    ScenarioResolution r -> scope "resolutions" do
      defeated <- select DefeatedInvestigator
      unless (null defeated) do
        resolutionOnly defeated $ scope "investigatorDefeat" $ setTitle "title" >> p "body"
        for_ defeated (kill attrs)
      remaining <- selectAny UneliminatedInvestigator
      resigned <- selectAny ResignedInvestigator
      if remaining || resigned
        then case r of
          NoResolution -> do_ R3
          _ -> do_ msg
        else gameOver
      pure s
    Do (ScenarioResolution r) -> scope "resolutions" do
      case r of
        Resolution 1 -> do
          captives <- selectJust $ assetIs Assets.theCaptives
          damage <- fieldMap AssetTokens (countTokens Damage) captives
          resolutionFlavor do
            setTitle "resolution1.title"
            p "resolution1.body"
            ul do
              li.validate (damage == 0) "resolution1.xp5"
              li.validate (damage >= 1 && damage <= 2) "resolution1.xp4"
              li.validate (damage >= 3) "resolution1.xp3"
              li "resolution1.captivesSaved"
              li "resolution1.skipToResolution4"
          record TheCaptivesWereSaved
          let bonus
                | damage == 0 = toBonus "bonus" 5
                | damage <= 2 = toBonus "bonus" 4
                | otherwise = toBonus "bonus" 3
          interludeXpAll bonus
          do_ R4
        Resolution 2 -> do
          captives <- selectJust $ assetIs Assets.theCaptives
          damage <- fieldMap AssetTokens (countTokens Damage) captives
          resolutionFlavor do
            setTitle "resolution2.title"
            p "resolution2.body"
            ul do
              li.validate (damage >= 5 && damage <= 6) "resolution2.xp3"
              li.validate (damage >= 7 && damage <= 8) "resolution2.xp2"
              li.validate (damage >= 9) "resolution2.xp1"
              li "resolution2.captivesLost"
              li "resolution2.skipToResolution4"
          record ManyCaptivesWereLost
          let bonus
                | damage <= 6 = toBonus "bonus" 3
                | damage <= 8 = toBonus "bonus" 2
                | otherwise = toBonus "bonus" 1
          interludeXpAll bonus
          do_ R4
        Resolution 3 -> do
          resolution "resolution3"
          record AllTheCaptivesWereLost
          eachInvestigator \iid -> searchCollectionForRandomBasicWeakness iid attrs [Madness]
          do_ R4
        Resolution 4 -> do
          ajaxControlled <- selectAny $ AssetControlledBy Anyone <> assetIs Assets.ajax
          when ajaxControlled do
            investigators <- allInvestigators
            forceAddCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.ajax
          investigators <- allInvestigators
          forceAddCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.drRosaMarquezBestInHerField
          record DrMarquezHasAPlan
          record $ AreasSurveyed SouthernFields
          resolutionWithXp "resolution4" $ allGainXp' attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheLongestNight <$> liftRunMessage msg attrs
