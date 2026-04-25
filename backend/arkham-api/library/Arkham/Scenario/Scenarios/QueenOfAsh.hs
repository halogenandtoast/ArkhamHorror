module Arkham.Scenario.Scenarios.QueenOfAsh (setupQueenOfAsh, queenOfAsh, QueenOfAsh (..)) where

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
import Arkham.Deck qualified as Deck
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy, inSkillTest)
import Arkham.Helpers.Xp
import Arkham.I18n (ikey)
import Arkham.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.EncounterCard (lookupEncounterCardDef)
import Arkham.Helpers.Modifiers (modifyEach)
import Arkham.Matcher hiding (InvestigatorDefeated, InvestigatorResigned)
import Arkham.Modifier (ModifierType (RevealAnotherChaosToken))
import Arkham.Investigator.Types (Field (..))
import Arkham.Trait qualified as Trait
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record, getRecordedCardCodes)
import Arkham.Name (nameTitle)
import Arkham.Id
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Scenarios.QueenOfAsh.Helpers
import Arkham.Strategy (FoundCardsStrategy (..), fromDeck)
import Arkham.Treachery.Cards qualified as Treacheries

newtype QueenOfAsh = QueenOfAsh ScenarioAttrs
  deriving stock Generic
  deriving anyclass IsScenario
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

instance HasModifiersFor QueenOfAsh where
  getModifiersFor (QueenOfAsh attrs) = do
    whenM inSkillTest $ do
      mAction <- getSkillTestAction
      case mAction of
        Just action | action `elem` [#fight, #evade] -> do
          mEid <- getSkillTestTargetedEnemy
          case mEid of
            Just eid -> do
              isCultist <- fieldP EnemyTraits (member Trait.Cultist) eid
              when isCultist $ modifyEach attrs [Cultist] [RevealAnotherChaosToken]
            Nothing -> pure ()
        _ -> pure ()

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
      if isEasyStandard attrs
        then pure $ toChaosTokenValue attrs Tablet (if atRitualSite then 4 else 2) 4
        else pure $ if atRitualSite then ChaosTokenValue Tablet AutoFailModifier else toChaosTokenValue attrs Tablet 4 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

setupQueenOfAsh :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupQueenOfAsh _attrs = do
  setup $ ul do
    li "gatherSets"
    li "placeLocations"
    li "setOutOfPlay"
    li "checkCampaignLog"
    li "placeDoom"
    li "shuffleRemainder"
    li "readyToBegin"

  gather Set.QueenOfAsh
  gather Set.AshenPilgrims
  gather Set.CosmicEvils
  gather Set.Cultists
  gather Set.Fire1
  gather Set.Hallucinations
  gather Set.ReekingDecay
  gather Set.Sewers
  gather Set.Torment

  _undergroundCistern <- place Locations.undergroundCistern

  sewerTunnels <- shuffleM
    [ Locations.sewerTunnelsInfestedPipes
    , Locations.sewerTunnelsOvergrownTunnels
    , Locations.sewerTunnelsFloodedCrypt
    , Locations.sewerTunnelsSmugglersCache
    , Locations.sewerTunnelsToxicWastePit
    ]
  sewerTunnelLids <- for (withIndex1 sewerTunnels) $ \(n, def) ->
    placeLabeled ("sewerTunnels" <> tshow n) def

  sewerCulvert <- place Locations.sewerCulvert

  setAside
    [ Treacheries.fire1
    , Treacheries.fire1
    , Treacheries.fire1
    , Treacheries.fire1
    , Treacheries.fire1
    , Enemies.elokossFaintEmbers
    , Enemies.elokossMotherOfFlame
    , Enemies.queensKnight
    , Enemies.heraldOfFlame
    , Assets.collector
    , Assets.collector
    ]

  setAside [Locations.sluiceControl]

  setAgendaDeck [Agendas.aGathering, Agendas.aRitual, Agendas.brethrenOfAsh]
  setActDeck [Acts.searchTheSewers, Acts.stopTheRIte]

  servantKilled <- getHasRecord InvestigatorsKilledTheServantOfFlame
  when servantKilled do
    servant <- fromGathered (cardIs Enemies.servantOfFlameAWillingSacrifice)
    for_ servant removeFromGame
  unless servantKilled $ setAside [Enemies.servantOfFlameAWillingSacrifice]

  scouredArkham <- getHasRecord InvestigatorsScouredArkhamForAnswers
  when scouredArkham $ eachInvestigator \iid -> gainClues iid (toSource _attrs) 1

  stirredUpTrouble <- getHasRecord InvestigatorsStirredUpTrouble
  when stirredUpTrouble $ do
    cultists <- fromGathered (#cultist <> CardFromEncounterSet Set.AshenPilgrims <> #enemy)
    cultists2 <- fromGathered (#cultist <> CardFromEncounterSet Set.Cultists <> #enemy)
    allCultists <- shuffle (cultists <> cultists2)
    n <- getPlayerCount
    let toPlace = take n allCultists
    for_ (zip sewerTunnelLids toPlace) \(loc, card) ->
      createEnemyAt_ card loc

  n <- getPlayerCount
  when (n >= 3) $ placeDoomOnAgenda 1

  discovered <- getHasRecord InvestigatorsDiscoveredTheCultsWhereabouts
  if discovered
    then pure ()
    else placeDoomOnAgenda 1

  startAt sewerCulvert

instance RunMessage QueenOfAsh where
  runMessage msg s@(QueenOfAsh attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro1" do
        unscoped $ setTitle "title"
        h_ "title"
        p "body"
        p "addTokens"

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

      discovered <- getHasRecord InvestigatorsDiscoveredTheCultsWhereabouts
      if discovered
        then flavor $ scope "intro2" do
          unscoped $ setTitle "title"
          h_ "title"
          p "body"
        else flavor $ scope "intro3" do
          unscoped $ setTitle "title"
          h_ "title"
          p "body"


      pure s
    Setup -> runScenarioSetup QueenOfAsh attrs $ setupQueenOfAsh attrs
    ScenarioSpecific "codex" v -> scope "codex" do
      let (_iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      case n of
        7 -> do
          servantCodes <- getRecordedCardCodes ServantOfElokoss
          let mHarbingerCode = listToMaybe servantCodes
          let servantName = maybe "unknown" (nameTitle . cdName . lookupEncounterCardDef) mHarbingerCode
          scope "servantOfFlame" do
            flavor $ withVars ["servantName" .= servantName] $ setTitle "title" >> p "intro"
            case mHarbingerCode of
              Just code | code == toCardCode Enemies.davidRenfieldDisillusionedEschatologist ->
                scope "davidRenfield" $ flavor $ p "body"
              Just code | code == toCardCode Enemies.corneliaAkelyExhaustedSupervisor ->
                scope "corneliaAkely" $ flavor $ p "body"
              Just code | code == toCardCode Enemies.naomiOBannionRunsThisTown ->
                scope "naomiOBannion" $ flavor $ p "body"
              Just code | code == toCardCode Enemies.sgtEarlMonroeDirtyCop ->
                scope "sgtEarlMonroe" $ flavor $ p "body"
              Just code | code == toCardCode Enemies.abigailForemanWaryLibrarian ->
                scope "abigailForeman" $ flavor $ p "body"
              Just code | code == toCardCode Enemies.margaretLiuBeguilingLoungeSinger ->
                scope "margaretLiu" $ flavor $ p "body"
              _ -> pure ()
            eachInvestigator \iid' -> case mHarbingerCode of
              Just code | code == toCardCode Enemies.davidRenfieldDisillusionedEschatologist -> do
                search iid' source iid' [fromDeck] (basic $ #tome <> #spell) (PlayFoundNoCost iid' 1)
              Just code | code == toCardCode Enemies.corneliaAkelyExhaustedSupervisor -> do
                healDamage iid' source 3
              Just code | code == toCardCode Enemies.naomiOBannionRunsThisTown -> do
                gainResources iid' source 5
              Just code | code == toCardCode Enemies.sgtEarlMonroeDirtyCop -> do
                search iid' source iid' [fromDeck] (basic #weapon) (PlayFoundNoCost iid' 1)
              Just code | code == toCardCode Enemies.abigailForemanWaryLibrarian -> do
                chooseOneM iid' do
                  labeled "Draw 1 card" $ drawCards iid' source 1
                  labeled "Draw 2 cards" $ drawCards iid' source 2
                  labeled "Draw 3 cards" $ drawCards iid' source 3
                  labeled "Draw 0 cards" nothing
              Just code | code == toCardCode Enemies.margaretLiuBeguilingLoungeSinger -> do
                healHorror iid' source 3
              _ -> pure ()
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
              else investigatorDefeated attrs iid
          resignedIids <- select ResignedInvestigator
          let xpDetails = concat
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
