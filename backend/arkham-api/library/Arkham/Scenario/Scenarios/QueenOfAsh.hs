module Arkham.Scenario.Scenarios.QueenOfAsh (queenOfAsh) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.BrethrenOfAsh.Import

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Exception
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (modifyEach)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy, inSkillTest)
import Arkham.Helpers.Xp
import Arkham.I18n (ikey)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (InvestigatorDefeated, InvestigatorResigned)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (getRecordedCardCodes, record, whenHasRecord)
import Arkham.Modifier (ModifierType (RevealAnotherChaosToken))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Scenarios.QueenOfAsh.Helpers
import Arkham.Strategy (FoundCardsStrategy (..), fromDeck)
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Xp

newtype QueenOfAsh = QueenOfAsh ScenarioAttrs
  deriving stock Generic
  deriving anyclass IsScenario
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

instance HasModifiersFor QueenOfAsh where
  getModifiersFor (QueenOfAsh attrs) = runMaybeT_ do
    liftGuardM inSkillTest
    action <- MaybeT getSkillTestAction
    guard $ action `elem` [#fight, #evade]
    eid <- MaybeT getSkillTestTargetedEnemy
    liftGuardM $ fieldP EnemyTraits (member Trait.Cultist) eid
    lift $ modifyEach attrs [Cultist] [RevealAnotherChaosToken]

queenOfAsh :: Difficulty -> QueenOfAsh
queenOfAsh difficulty =
  scenario
    QueenOfAsh
    "12168"
    "Queen of Ash"
    difficulty
    [ ".              .             sluiceControl       .             ."
    , ".              .             undergroundCistern  .             ."
    , "sewerTunnels1  sewerTunnels2 sewerTunnels3       sewerTunnels4 sewerTunnels5"
    , ".              .             sewerCulvert        .             ."
    ]

instance HasChaosTokenValue QueenOfAsh where
  getChaosTokenValue iid chaosTokenFace (QueenOfAsh attrs) = case chaosTokenFace of
    Skull -> do
      fireCount <- selectCount $ LocationWithTreachery (treacheryIs Treacheries.fire1)
      pure $ toChaosTokenValue attrs Skull fireCount (fireCount * 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> do
      atRitualSite <- selectAny $ LocationWithTrait Trait.RitualSite <> locationWithInvestigator iid
      pure
        $ if
          | isEasyStandard attrs -> toChaosTokenValue attrs Tablet (if atRitualSite then 4 else 2) 4
          | atRitualSite -> ChaosTokenValue Tablet AutoFailModifier
          | otherwise -> toChaosTokenValue attrs Tablet 4 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage QueenOfAsh where
  runMessage msg s@(QueenOfAsh attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> scope "intro" do
      difficulty <- getDifficulty
      discovered <- getHasRecord InvestigatorsDiscoveredTheCultsWhereabouts
      flavor do
        setTitle "title"
        h_ "title"
        p "intro1"
        ul do
          li.nested "addTokens" do
            li.validate (difficulty == Easy) "easy"
            li.validate (difficulty == Standard) "standard"
            li.validate (difficulty == Hard) "hard"
            li.validate (difficulty == Expert) "expert"
          li.nested "checkCampaignLog" do
            li.validate discovered "discovered"
            li.validate (not discovered) "otherwise"

      case attrs.difficulty of
        Easy -> addChaosToken ElderThing
        Standard -> do
          addChaosToken ElderThing
          addChaosToken Tablet
        Hard -> do
          addChaosToken Skull
          addChaosToken Tablet
          addChaosToken ElderThing
        Expert -> do
          addChaosToken Cultist
          addChaosToken Tablet
          addChaosToken ElderThing
          addChaosToken Skull

      flavor do
        setTitle "title"
        p $ if discovered then "intro2" else "intro3"

      pure s
    Setup -> runScenarioSetup QueenOfAsh attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeUndergroundCistern" do
          li "sewers"
          li "sluiceControl"
          li "startAt"
        li.nested "checkCampaignLog" do
          li "scouredArkhamForAnswers"
          li "stirredUpTrouble"
          li "killedServantOfFlame"
        li "placeDoom"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.QueenOfAsh
      gather Set.AshenPilgrims
      gather Set.CosmicEvils
      gather Set.Cultists
      gather Set.Fire1
      gather Set.Hallucinations
      gather Set.ReekingDecay
      gather Set.Sewers
      gather Set.Torment

      setAgendaDeck [Agendas.aGathering, Agendas.aRitual, Agendas.brethrenOfAsh]
      setActDeck [Acts.searchTheSewers, Acts.stopTheRIte]

      place_ Locations.undergroundCistern

      sewerTunnels <-
        placeGroupCapture "sewerTunnels"
          =<< shuffle
            [ Locations.sewerTunnelsInfestedPipes
            , Locations.sewerTunnelsOvergrownTunnels
            , Locations.sewerTunnelsFloodedCrypt
            , Locations.sewerTunnelsSmugglersCache
            , Locations.sewerTunnelsToxicWastePit
            ]

      startAt =<< place Locations.sewerCulvert

      setAside
        [ Treacheries.fire1
        , Treacheries.fire1
        , Treacheries.fire1
        , Treacheries.fire1
        , Treacheries.fire1
        , Enemies.elokossFaintEmbers
        , Enemies.queensKnight
        , Enemies.heraldOfFlame
        , Assets.collector
        , Assets.collector
        ]

      setAside [Locations.sluiceControl]

      servantKilled <- getHasRecord InvestigatorsKilledTheServantOfFlame
      if servantKilled
        then do
          servant <- fromGathered (cardIs Enemies.servantOfFlameAWillingSacrifice)
          for_ servant removeFromGame
        else setAside [Enemies.servantOfFlameAWillingSacrifice]

      whenHasRecord InvestigatorsScouredArkhamForAnswers do
        eachInvestigator \iid -> gainClues iid (toSource attrs) 1

      n <- getPlayerCount

      whenHasRecord InvestigatorsStirredUpTrouble do
        cultists <-
          shuffle
            =<< fromGathered (#cultist <> mapOneOf CardFromEncounterSet [Set.AshenPilgrims, Set.Cultists] <> #enemy)
        for_ (zip (take n cultists) sewerTunnels) (uncurry createEnemyAt_)

      when (n >= 3) $ placeDoomOnAgenda 1

      discovered <- getHasRecord InvestigatorsDiscoveredTheCultsWhereabouts
      unless discovered $ placeDoomOnAgenda 1
    ScenarioSpecific "codex" v -> scope "codex" do
      let (_iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      case n of
        Phi -> do
          servantCodes <- getRecordedCardCodes ServantOfElokoss
          for_ (headMay servantCodes) \code -> do
            scope "servantOfFlame" do
              flavor
                $ setTitle "title"
                >> compose.codex
                  ( h_ "title"
                      >> p "intro"
                      >> hr
                      >> if
                        | code == Enemies.davidRenfieldDisillusionedEschatologist.cardCode ->
                            p "davidRenfield.body"
                        | code == Enemies.corneliaAkelyExhaustedSupervisor.cardCode ->
                            p "corneliaAkely.body"
                        | code == Enemies.naomiOBannionRunsThisTown.cardCode ->
                            p "naomiOBannion.body"
                        | code == Enemies.sgtEarlMonroeDirtyCop.cardCode ->
                            p "sgtEarlMonroe.body"
                        | code == Enemies.abigailForemanWaryLibrarian.cardCode ->
                            p "abigailForeman.body"
                        | otherwise -> p "margaretLiu.body"
                  )
              eachInvestigator \iid' ->
                if
                  | code == Enemies.davidRenfieldDisillusionedEschatologist.cardCode -> do
                      chooseOneM iid' do
                        labeled' "davidRenfield.search" do
                          search iid' source iid' [fromDeck] (basic $ #tome <> #spell) (PlayFoundNoCost iid' 1)
                        unscoped skip_
                  | code == Enemies.corneliaAkelyExhaustedSupervisor.cardCode -> do
                      healDamageIfCan iid' source 3
                  | code == Enemies.naomiOBannionRunsThisTown.cardCode -> do
                      gainResources iid' source 5
                  | code == Enemies.sgtEarlMonroeDirtyCop.cardCode -> do
                      chooseOneM iid' do
                        labeled' "sgtEarlMonroe.search"
                          $ search iid' source iid' [fromDeck] (basic #weapon) (PlayFoundNoCost iid' 1)
                        unscoped skip_
                  | code == Enemies.abigailForemanWaryLibrarian.cardCode -> do
                      chooseOneM iid' $ unscoped do
                        for_ [1 .. 3] \x -> do
                          countVar x $ labeled' "drawCards" $ drawCards iid' source x
                        countVar 0 $ labeled' "drawCards" nothing
                  | otherwise -> healHorror iid' source 3
        _ -> error "invalid codex entry"
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          record ElokossWasReborn
          eachInvestigator \iid -> investigatorDefeated attrs iid
          xp <- allGainXp' attrs
          resolutionFlavor $ withVars ["xp" .= xp] $ setTitle "noResolution.title" >> p "noResolution.body"
        Resolution 1 -> do
          record InvestigatorsDefeatedElokossAndTheBrethrenOfAsh
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" 5
          eachInvestigator \iid -> do
            sufferMentalTrauma iid 2
            sufferPhysicalTrauma iid 2
          resolutionFlavor $ withVars ["xp" .= xp] $ setTitle "resolution1.title" >> p "resolution1.body"
        Resolution 2 -> do
          record InvestigatorsStoppedElokosssGloriousRebirth
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" 5
          eachInvestigator \iid -> do
            sufferMentalTrauma iid 2
            sufferPhysicalTrauma iid 2
          resolutionFlavor $ withVars ["xp" .= xp] $ setTitle "resolution2.title" >> p "resolution2.body"
        Resolution 3 -> do
          record InvestigatorsFloodedTheBrethrenOfAshsSummoningRitual
          victoryXp <- getInitialVictory
          eachInvestigator \iid -> do
            resigned <- field InvestigatorResigned iid
            if resigned
              then do
                sufferMentalTrauma iid 3
                push $ GainXP iid (toSource attrs) (victoryXp + 3)
              else kill attrs iid
          resignedIids <- select ResignedInvestigator
          let xpDetails =
                concat
                  [ [ InvestigatorGainXp iid (XpDetail XpFromVictoryDisplay "Victory Display" victoryXp)
                    , InvestigatorGainXp iid (XpDetail XpFromCardEffect ("$" <> ikey "resolution3") 3)
                    ]
                  | iid <- resignedIids
                  ]
          push $ ReportXp $ XpBreakdown xpDetails
          let totalXp = victoryXp + 3
          resolutionFlavor $ withVars ["xp" .= totalXp] $ setTitle "resolution3.title" >> p "resolution3.body"
        other -> throwIO $ UnknownResolution other

      endOfScenario
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (token.face == ElderThing) $ do
        findTopOfDiscard (cardIs Treacheries.fire1) >>= traverse_ (drawCardFrom iid Deck.EncounterDiscard)
      QueenOfAsh <$> liftRunMessage msg attrs
    _ -> QueenOfAsh <$> liftRunMessage msg attrs
