module Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman (theMyriadGentleman) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.DummyDeck (setUpDummyDeck)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.Id (EnemyId)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype TheMyriadGentleman = TheMyriadGentleman ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMyriadGentleman :: Difficulty -> TheMyriadGentleman
theMyriadGentleman difficulty =
  scenario
    TheMyriadGentleman
    ":ages-unwound:023"
    "The Myriad Gentleman"
    difficulty
    [ "hourglass star     .      ."
    , "heart     squiggle circle triangle"
    , "moon      equals   diamond square"
    ]

setAsideLocations :: [CardDef]
setAsideLocations =
  [ Locations.entranceHall
  , Locations.parlor
  , Locations.kitchen
  , Locations.landing
  , Locations.masterBedroom
  , Locations.study
  ]

instance HasChaosTokenValue TheMyriadGentleman where
  getChaosTokenValue iid tokenFace (TheMyriadGentleman attrs) = case tokenFace of
    Skull -> do
      atYour <- selectAny $ EnemyWithTrait Myriad <> enemyAtLocationWith iid
      pure
        $ toChaosTokenValue attrs Skull (if atYour then 3 else 1) (if atYour then 4 else 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheMyriadGentleman where
  runMessage msg s@(TheMyriadGentleman attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      accepted <- getHasRecord TheInvestigatorsAcceptedAnOfferOfHelp
      flavor $ h "title" >> p "checkCampaignLog"
      flavor $ h "title" >> p (if accepted then "intro1" else "intro2")
      flavor $ h "title" >> p "intro3"
      pure s
    Setup -> runScenarioSetup TheMyriadGentleman attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setAsideLocations"
        li "setAsideCards"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      additionalRules "copiesOfEnemies"

      gather Set.TheMyriadGentleman
      gather Set.ShiftingReality
      gather Set.UnravellingYears
      gather Set.AgentsOfYogSothoth
      gather Set.LockedDoors

      setAgendaDeck [Agendas.ripplesInReality, Agendas.armyOfOne, Agendas.fromOneMany]
      setActDeck [Acts.aFineCountryGarden, Acts.justOneMan, Acts.swarmed]

      placeAll [Locations.hedgeMaze, Locations.ornateFountain, Locations.stables]
      startAt =<< place Locations.lawn

      setAside setAsideLocations
      setAside [Enemies.theMyriadGentleman_042, Enemies.theMyriadGentleman_043]
      setAsideEvery
        $ mapOneOf
          cardIs
          [Treacheries.exUnoPlures, Treacheries.theyJustKeepComing, Treacheries.notWelcomeHere]
      setAside [Assets.aforgomonsBlade]
      setAside [Treacheries.gazeOfAforgomon]

      setMeta emptyMyriadMeta

      setUpDummyDeck

      unlessM (getHasRecord TheInvestigatorsAcceptedAnOfferOfHelp) $ placeDoomOnAgenda 1
    Msg.PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      myriad <- select $ EnemyWithTrait Myriad <> enemyAtLocationWith iid
      chooseTargetM iid myriad $ nonAttackEnemyDamage (Just iid) Cultist 1
      pure s
    Msg.FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Tablet
      , isEasyStandard attrs -> do
          whenM onActTwoOrThree $ spawnMyriadCopiesEngagedWith iid 1
          pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      withSkillTest \sid -> afterThisTestResolves sid do
        whenM onActTwoOrThree $ spawnMyriadCopiesEngagedWith iid 1
      pure s
    ScenarioSpecific key v | key == spawnMyriadCopiesKey -> do
      mintMyriadCopies (toResult @MyriadSpawnRequest v)
      pure s
    ScenarioSpecific key v | key == registerMyriadCopyKey -> do
      let copy = toResult @MyriadCopy v
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      pure $ TheMyriadGentleman $ attrs & metaL .~ toJSON (MyriadMeta $ copy : meta.copies)
    Msg.RemoveEnemy eid -> do
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      for_ (find (\c -> c.enemy == eid && not c.returned) meta.copies) \copy -> do
        scenarioSpecific returnedMyriadCopyKey eid
        push
          $ Msg.PutCardOnBottomOfDeck copy.owner (Deck.InvestigatorDeck copy.owner) copy.card
      TheMyriadGentleman <$> liftRunMessage msg attrs
    ScenarioSpecific key v | key == returnedMyriadCopyKey -> do
      let eid = toResult @EnemyId v
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      let mark c = if c.enemy == eid then c {myriadCopyReturned = True} else c
      pure $ TheMyriadGentleman $ attrs & metaL .~ toJSON (MyriadMeta $ map mark meta.copies)
    Msg.Discarded (EnemyTarget eid) _ _ -> do
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      if any ((== eid) . (.enemy)) meta.copies
        then pure s
        else TheMyriadGentleman <$> liftRunMessage msg attrs
    ScenarioResolution res -> scope "resolutions" do
      case res of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          record TheInvestigatorsLearntOfTheMyriadsRitual
          resolutionWithXp "resolution1" $ allGainXp' attrs
          offerAforgomonsBlade
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsFledTheGentlemansManor
          resolutionWithXp "resolution2" $ allGainXp' attrs
          offerAforgomonsBlade
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheMyriadGentleman <$> liftRunMessage msg attrs

offerAforgomonsBlade :: ReverseQueue m => m ()
offerAforgomonsBlade = do
  defeated <-
    selectAny
      $ VictoryDisplayCardMatch
      $ basic (cardIs Enemies.theMyriadGentleman_043)
  when defeated do
    iids <- allInvestigators
    gaze <- fetchCard Treacheries.gazeOfAforgomon
    addCampaignCardToDeckChoiceAlsoShowing iids DoNotShuffleIn Assets.aforgomonsBlade [gaze] \iid ->
      [Msg.AddCampaignCardToDeck iid DoNotShuffleIn gaze]
