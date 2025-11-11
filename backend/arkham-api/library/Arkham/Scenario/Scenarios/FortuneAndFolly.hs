module Arkham.Scenario.Scenarios.FortuneAndFolly (fortuneAndFolly) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetCardCode))
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (Discarded, enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Message.Story
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token
import Arkham.Window qualified as Window

newtype FortuneAndFolly = FortuneAndFolly ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortuneAndFolly :: Difficulty -> FortuneAndFolly
fortuneAndFolly difficulty =
  scenario
    FortuneAndFolly
    "88001"
    "Fortune And Folly"
    difficulty
    [ "squiggle .      t"
    , "plus     .      triangle"
    , "square   circle diamond"
    ]

instance HasChaosTokenValue FortuneAndFolly where
  getChaosTokenValue iid tokenFace (FortuneAndFolly attrs) = case tokenFace of
    Skull -> do
      alarmLevel <- getAlarmLevel iid
      pure $ toChaosTokenValue attrs Skull ((alarmLevel + 1) `div` 2) alarmLevel
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs Tablet 5 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FortuneAndFolly where
  runMessage msg s@(FortuneAndFolly attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      c <- selectOne TheCampaign
      flavor do
        setTitle "title"
        p "intro1"
        p.basic.right.validate (c == Just "08") "scarletKeys"
        p.basic.right.validate (c /= Just "08") "notScarletKeys"
      if c == Just "08"
        then doStep 2 PreScenarioSetup
        else doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro4") do
        labeled' "skip" $ doStep (-1) PreScenarioSetup
        labeled' "doNotSkip" $ doStep 5 PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro5"
      pure s
    DoStep (-1) PreScenarioSetup -> do
      pure $ FortuneAndFolly $ attrs & setMetaKey "skip" True
    Setup -> runScenarioSetup FortuneAndFolly attrs do
      gather Set.FortuneAndFolly
      gatherAndSetAside Set.FortunesChosen
      gatherAndSetAside Set.PlanInShambles

      setAgendaDeck [Agendas.theHouseAlwaysWatches]
      setActDeck [Acts.casingTheJoint]
      casinoFloor <- place Locations.casinoFloorCalmNight

      startAt casinoFloor
      theStakeout <- genCard Stories.theStakeout
      push $ PlaceStory theStakeout (AtLocation casinoFloor)

      baccaratTable <- place Locations.baccaratTable
      assetAt_ Assets.isamaraOrdonezLoungeSingerInconspicious baccaratTable

      highRollersTable <- place Locations.highRollersTableBusyNight
      enemyAt_ Enemies.abarranArrigorriagakoaTheManWithTheRubyRing highRollersTable

      rouletteWheel <- place Locations.rouletteWheel
      casinoGuard <-
        (`enemyAt` rouletteWheel)
          =<< sampleOneOf (Enemies.casinoGuardA :| [Enemies.casinoGuardB, Enemies.casinoGuardC])
      ifTheUniformFits <- genCard Stories.ifTheUniformFits
      push $ PlaceStory ifTheUniformFits (AttachedToEnemy casinoGuard)

      placeAll
        [ Locations.casinoLoungeCalmNight
        , Locations.pokerTable
        , Locations.slotMachines
        ]

      setAside
        [ Agendas.openingHand
        , Agendas.theTurn
        , Agendas.allBetsDown
        , Acts.theTake
        , Acts.theExit
        , Locations.staffAccessHallway
        , Locations.securityOffice
        , Locations.guardRoom
        , Locations.ownersOffice
        , Locations.countingRoom
        , Locations.vaultDoor
        , Locations.relicRoomSanctumOfFortune
        , Stories.fortunesDisfavor25
        , Stories.fortunesDisfavor26
        , Stories.fortunesDisfavor27
        , Assets.cashCart
        , Assets.deckOfPossibilitiesTychokineticImplement
        , Assets.isamaraOrdonezTheTorchSinger
        ]

      theWellspringOfFortune <- createScarletKeyAt Keys.theWellspringOfFortune NextToAct
      placeTokens attrs theWellspringOfFortune Clue =<< perPlayer 7

      eachInvestigator \iid -> placeTokens attrs iid AlarmLevel 1

      n <- getPlayerCount
      if n == 1
        then do
          iid <- getLead
          chooseNM iid 2 do
            cardsLabeled
              [ Assets.theFaceUnpracticed
              , Assets.theMuscleUnpracticed
              , Assets.theThiefUnpracticed
              , Assets.theGrifterUnpracticed
              ]
              \card -> createAssetAt_ card (InPlayArea iid)
        else eachInvestigator (`forInvestigator` msg)
    ForInvestigator iid Setup -> do
      inPlayAlready <-
        selectField AssetCardCode
          $ mapOneOf
            assetIs
            [ Assets.theFaceUnpracticed
            , Assets.theMuscleUnpracticed
            , Assets.theThiefUnpracticed
            , Assets.theGrifterUnpracticed
            ]
      chooseOneM iid do
        cardsLabeled
          ( filter
              ((`notElem` inPlayAlready) . toCardCode)
              [ Assets.theFaceUnpracticed
              , Assets.theMuscleUnpracticed
              , Assets.theThiefUnpracticed
              , Assets.theGrifterUnpracticed
              ]
          )
          \card -> createAssetAt_ card (InPlayArea iid)

      pure s
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      withLocationOf iid \loc -> do
        enemies <- select $ NearestEnemyTo iid AnyEnemy
        chooseTargetM iid enemies \enemy -> moveTowards Tablet enemy loc
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      chooseOneM iid do
        labeled' "elderThing.alarm" do
          raiseAlarmLevel ElderThing [iid]
          passSkillTest
        skip_
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> raiseAlarmLevel Cultist [iid]
        Tablet | isEasyStandard attrs -> do
          withLocationOf iid \loc -> do
            enemies <- select $ NearestEnemyTo iid AnyEnemy
            chooseTargetM iid enemies \enemy -> moveTowards Tablet enemy loc
        _ -> pure ()
      pure s
    ScenarioSpecific "mulligan" val -> fmap FortuneAndFolly do
      let params = toResult @CheckGameIcons val
      if null params.cards
        then scenarioSpecific "checkGameIcons" params
        else focusCards params.cards do
          chooseOneM params.investigator do
            labeled' "keepHand" $ scenarioSpecific "checkGameIcons" params
            for_ (eachWithRest params.cards) \(card, rest) ->
              targeting card $ scenarioSpecific "mulligan" params {cards = rest}
      pure attrs
    ScenarioSpecific "checkGameIcons" val -> fmap FortuneAndFolly do
      let params = toResult @CheckGameIcons val
      let toFind = params.n - count (isJust . toPlayingCard) params.cards
      if toFind == 0
        then do
          if params.mulligan == CanMulligan
            then scenarioSpecific "mulligan" params {mulligan = NoMulligan}
            else
              push $ DiscardedCards params.investigator ScenarioSource params.target $ toCard <$> params.cards
          pure attrs
        else case attrs.encounterDeck of
          Deck [] -> do
            unless (scenarioInShuffle attrs) do
              checkWhen Window.EncounterDeckRunsOutOfCards
              shuffleEncounterDiscardBackIn
              push msg
            pure attrs
          Deck (card : deck) -> do
            checkWhen $ Window.Discarded (Just params.investigator) ScenarioSource (toCard card)
            push $ Discarded (CardIdTarget card.id) ScenarioSource (toCard card)
            checkAfter $ Window.Discarded (Just params.investigator) ScenarioSource (toCard card)
            push $ ScenarioSpecific "checkGameIcons" $ toJSON params {cards = card : params.cards}
            pure $ attrs & encounterDeckL .~ Deck deck & discardL %~ (card :)
    _ -> FortuneAndFolly <$> liftRunMessage msg attrs
