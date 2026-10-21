module Arkham.Scenario.Scenarios.TracesToNowhere (tracesToNowhere) where

import Arkham.Act.CardDefs.TracesToNowhere qualified as Acts
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.CardDefs.TracesToNowhere qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types (Field (..))
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Exception
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Campaign hiding (addCampaignCardToDeckChoice)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message (pattern SetCampaignChaosBag)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Ally, Arkham, Monster, Otherworld, Suspect, Syndicate, Witness))

newtype TracesToNowhere = TracesToNowhere ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tracesToNowhere :: Difficulty -> TracesToNowhere
tracesToNowhere difficulty =
  sideStory TracesToNowhere "73001" "Traces to Nowhere" difficulty initialLayout

instance HasChaosTokenValue TracesToNowhere where
  getChaosTokenValue iid tokenFace (TracesToNowhere attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> do
      otherworld <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Otherworld
      pure $ toChaosTokenValue attrs Tablet (if otherworld then 4 else 2) (if otherworld then 5 else 3)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusThree
  , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour
  , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour , MinusFive
  , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusThree , MinusFour , MinusFive
  , MinusSeven , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , AutoFail
  , ElderSign
  ]
{- FOURMOLU_ENABLE -}

scenarioTokens :: Difficulty -> [ChaosTokenFace]
scenarioTokens = \case
  Easy -> easyTokens
  Standard -> standardTokens
  Hard -> hardTokens
  Expert -> expertTokens

instance RunMessage TracesToNowhere where
  runMessage msg s@(TracesToNowhere attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor do
        h "title"
        p "body"
        unscoped $ p.right "proceedToSetup"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ scenarioTokens attrs.difficulty
      pure s
    Setup -> runScenarioSetup TracesToNowhere attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "beginPlay"
        li "spawnEnforcer"
        li "abductAziza"
        li.nested "setOutOfPlay" do
          li "suspects"
          li "culprits"
          li "actAgenda"
          li "locations"
          li "witnesses"
          li "dimensionalMadness"
        unscoped $ li "shuffleRemainder"
        li.nested "leadsDeck" do
          li "leadsDeckNote"
          li "leadsDeckPlacement"
        li "begin"

      -- "Always use the Standalone Mode chaos bag for this scenario."
      unlessStandalone do
        bag <- campaignField CampaignChaosBag
        setGlobal CampaignTarget "tracesToNowhereSavedChaosBag" bag
        setChaosTokens $ scenarioTokens attrs.difficulty

      gather Set.TracesToNowhere
      gatherAndSetAside Set.DimensionalMadness

      steersOffice <- place Locations.steersOffice
      startAt steersOffice
      merchantDistrict <- place Locations.merchantDistrict
      placeAll [Locations.frenchHill, Locations.southChurch, Locations.historicalSociety]

      createEnemyAt_ Enemies.hiredEnforcer merchantDistrict

      -- "Aziza Steer has been abducted!"
      abduct . only =<< fetchCard Assets.azizaSteerHasABoneToPick

      setAside
        [ Assets.albrechtHusserlRecklessGambler
        , Assets.eliasHusserlJrFleeingTheCity
        , Assets.eugenePeelHidingSomething
        , Assets.linaHusserlTheNextBigThing
        , Assets.matildeCorreaInvestigativeReporter
        , Assets.vicarYangTryingToDoTheRightThing
        , Enemies.crookedCops
        , Enemies.albrechtHusserlDesperateGambler
        , Enemies.eugenePeelDoubleCrossingCultist
        , Enemies.linaHusserlVisionsOfHerOwnDemise
        , Agendas.intoThePit
        , Acts.linasNightmare
        , Acts.mrPeelsScheme
        , Acts.albrechtsGamble
        , Locations.arkhamAdvertiser
        , Locations.trainStation
        , Locations.tickTockClub
        , Locations.duterteFuneralHomeAfterHours
        , Locations.duterteFuneralHomeSomethingInTheBasement
        , Locations.meigerEstateUnderNewOwnership
        , Locations.meigerEstateBeyondTheBlackDoor
        ]

      witnesses <-
        traverse
          fetchCard
          [ Assets.adeleHusserlEstrangedDaughter
          , Assets.josephineHusserlMiffedMatriarch
          , Assets.charlesTammJustHereToGetPaid
          , Assets.gladysWashingtonCuriousCoroner
          ]

      setAgendaDeck [Agendas.hardCase, Agendas.fromAnotherDimension, Agendas.runningOutOfTime]
      setActDeck [Acts.takingOverTheSearch, Acts.moreThanYouBargainedFor, Acts.theCulprit]

      {- "Create the Leads deck by shuffling the 4 set-aside Witness story assets
      along with the top 2 cards of the encounter deck (top 3 cards instead if
      there are 3 or 4 investigators in the game)." -}
      n <- getPlayerCount
      top <- take (if n >= 3 then 3 else 2) . map toCard . unDeck <$> getEncounterDeck
      for_ top obtainCard
      push . SetScenarioDeck LeadsDeck =<< shuffle (witnesses <> top)
    ResolveChaosToken _ Cultist iid -> do
      whenAny (EnemyWithTrait Syndicate <> at_ (locationWithInvestigator iid))
        $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull -> when (isEasyStandard attrs) $ monsterAttacks iid
        ElderThing ->
          if attrs.difficulty `elem` [Easy, Standard]
            then do
              -- "deal 1 horror to a Witness asset in play"
              witnesses <- select $ AssetWithTrait Witness
              chooseTargetM iid witnesses \witness -> dealAssetHorror witness ElderThing 1
            else do
              -- "choose an Ally or Witness asset. Something abducts that asset."
              targets' <- select $ mapOneOf AssetWithTrait [Ally, Witness]
              chooseTargetM iid targets' abductAsset
        _ -> pure ()
      pure s
    ResolveChaosToken _ Skull iid -> do
      when (isHardExpert attrs) $ afterSkillTestQuiet $ monsterAttacks iid
      pure s
    {- Interlude: The Culprit. Reached from codex E, H or J, each of which
    remembers which culprit it was before skipping here. -}
    ScenarioSpecific "theCulprit" _ -> scope "interlude" do
      flavor $ h "theCulprit1.title" >> p "theCulprit1.body"

      -- "Put each set-aside Otherworld location into play."
      traverse_ placeLocation_
        =<< getSetAsideCardsMatching (#location <> CardWithTrait Otherworld)

      bonePit <- selectJust $ locationIs Locations.bonePit
      eachInvestigator \iid -> moveTo attrs iid bonePit

      {- "Flip Abductor to its Dimensional Shambler side and move it to Bone Pit
      (or spawn it at Bone Pit, if it is set-aside)." It is one enemy with two
      faces, so an in-play Abductor flips in place and keeps its identity. -}
      lead <- getLead
      selectOne (enemyIs Enemies.abductorThatWhichLurksInShadows) >>= \case
        Just abductor -> do
          flipOverBy lead attrs abductor
          enemyMoveTo attrs abductor bonePit
        Nothing -> do
          card <- fetchCard Enemies.dimensionalShamblerDraggingThemBelow
          createEnemyAt_ card bonePit

      {- "Add the set-aside agenda 4a to the agenda deck. Advance directly to
      agenda 4a (do not resolve any other agendas)." -}
      push . AddAgenda 4 =<< genCard Agendas.intoThePit
      push $ AdvanceToAgenda 4 Agendas.intoThePit Agenda.A (toSource attrs)

      doStep 1 msg
      pure s
    DoStep 1 (ScenarioSpecific "theCulprit" _) -> scope "interlude" do
      {- Each branch reads its own key rather than falling through, so a state
      where none was remembered runs nothing instead of the wrong culprit. -}
      whenM (remembered AlbrechtIsTheCulprit)
        $ culpritBranch
          attrs
          "theCulprit2"
          Assets.albrechtHusserlRecklessGambler
          Enemies.albrechtHusserlDesperateGambler
          Acts.albrechtsGamble
      whenM (remembered LinaOpenedTheDoor)
        $ culpritBranch
          attrs
          "theCulprit3"
          Assets.linaHusserlTheNextBigThing
          Enemies.linaHusserlVisionsOfHerOwnDemise
          Acts.linasNightmare
      whenM (remembered EugeneIsNotWhatHeSeems)
        $ culpritBranch
          attrs
          "theCulprit4"
          Assets.eugenePeelHidingSomething
          Enemies.eugenePeelDoubleCrossingCultist
          Acts.mrPeelsScheme
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          record TheInvestigatorsFailedToSolveTheCase
          gameOver
        Resolution 1 -> do
          resolution "resolution1"
          record TheInvestigatorsSavedLinaHusserl
          push R5
        Resolution 2 -> do
          resolution "resolution2"
          record TheInvestigatorsStoppedEugenePeel
          push R5
        Resolution 3 -> do
          resolution "resolution3"
          record TheInvestigatorsStoppedAlbrechtHusserl
          push R5
        Resolution 4 -> do
          resolution "resolution4"
          record TheCulpritIsStillAtLarge
          push R5
        Resolution 5 -> do
          resolution "resolution5"
          do_ msg
        _ -> throw $ UnknownResolution r
      pure s
    Do (ScenarioResolution _) -> scope "resolutions" do
      {- "If the Aziza Steer story asset was under an investigator's control when
      the scenario ended, the investigators have earned the Aziza Steer
      (Hardboiled) campaign reward card." -}
      rescued <- selectAny $ AssetControlledBy Anyone <> assetIs Assets.azizaSteerHasABoneToPick
      when rescued do
        investigators <- allInvestigators
        addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.azizaSteerHardboiled

      {- "Each investigator earns experience equal to the victory X value of each
      card in the victory display. For each story asset in the victory display
      when the scenario ended, choose an investigator to earn 1 bonus experience." -}
      stories <-
        selectCount $ VictoryDisplayCardMatch $ basic (#asset <> mapOneOf CardWithTrait [Suspect, Witness])
      resolutionWithXp "fin" $ allGainXpWithBonus' attrs (toBonus "bonus.storyAssets" stories)

      unlessStandalone do
        stored @[ChaosTokenFace] "tracesToNowhereSavedChaosBag"
          >>= traverse_ (push . SetCampaignChaosBag)

      endOfScenario
      pure s
    _ -> TracesToNowhere <$> liftRunMessage msg attrs

-- | "If ... there is a {Monster} enemy at your location, that enemy attacks you."
monsterAttacks :: ReverseQueue m => InvestigatorId -> m ()
monsterAttacks iid = do
  monsters <- select $ EnemyWithTrait Monster <> enemyAtLocationWith iid
  chooseTargetM iid monsters \enemy -> initiateEnemyAttack enemy Skull iid

{- | The Culprit 2, 3 and 4 differ only in which culprit stepped out of the pit:
the story asset leaves the game, its enemy spawns at Bone Pit, every Suspect
still in Arkham and the whole Leads deck are abducted, and the gathered pile is
split between the Dimensional Shambler and Bone Pit before the matching act 4a
is advanced to.
-}
culpritBranch
  :: (HasI18n, ReverseQueue m)
  => ScenarioAttrs -> Scope -> CardDef -> CardDef -> CardDef -> m ()
culpritBranch attrs sect storyAsset culpritEnemy act = scope sect do
  flavor $ h "title" >> p "body"

  bonePit <- selectJust $ locationIs Locations.bonePit
  selectForMaybeM (assetIs storyAsset) (removeFromGame . AssetTarget)
  culpritCard <- fetchCard culpritEnemy
  createEnemyAt_ culpritCard bonePit

  {- "Something abducts each Suspect story asset in play at an Arkham location,
  and each card in the Leads deck." -}
  selectEach (AssetWithTrait Suspect <> AssetAt (LocationWithTrait Arkham)) abductAsset
  leads <- getLeadsDeck
  for_ leads \card -> push $ RemoveCardFromScenarioDeck LeadsDeck card
  abduct leads

  {- "Gather all abducted cards and shuffle them together. Attach 1 [per player]
  of these cards, at random, to Dimensional Shambler. Place the remaining cards
  facedown beneath Bone Pit." -}
  gathered <- shuffle =<< getAbductedCards
  n <- perPlayer 1
  let (attached, beneath) = splitAt n gathered
  shambler <- selectJust $ enemyIs Enemies.dimensionalShamblerDraggingThemBelow
  push $ RemoveFromUnderneath ScenarioTarget gathered
  placeUnderneath shambler attached
  placeUnderneath bonePit beneath

  -- "Advance directly to act 4a (do not resolve act 3b)."
  push . AddAct 4 =<< genCard act
  push $ AdvanceToAct 4 act Act.A (toSource attrs)
