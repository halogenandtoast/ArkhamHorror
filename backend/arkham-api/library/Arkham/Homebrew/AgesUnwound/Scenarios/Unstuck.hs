module Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck (unstuck) where

import Arkham.Card
import Arkham.Direction
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.FlavorText (flavor, h, li, p, resolutionOnly, setup, ul, withTitle)
import Arkham.Helpers.Query (allInvestigators, getSetAsideCardsMatching)
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (RegularEncounterDeck))
import Arkham.Scenario.Import.Lifted

newtype Unstuck = Unstuck ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unstuck :: Difficulty -> Unstuck
unstuck difficulty =
  scenario
    Unstuck
    ":ages-unwound:062"
    "Unstuck"
    difficulty
    [ ".         .         .         location1  .        .         ."
    , ".         location8 location8 location1  location2 location2 ."
    , ".         location8 location8 .          location2 location2 ."
    , "location7 location7 .         timestream arkham    location3 location3"
    , ".         location6 location6 .          location4 location4 ."
    , ".         location6 location6 location5  location4 location4 ."
    , ".         .         .         location5  .        .         ."
    ]

instance HasChaosTokenValue Unstuck where
  getChaosTokenValue iid tokenFace (Unstuck attrs) = case tokenFace of
    Skull -> do
      ag <- getCurrentAgendaStep
      ac <- getCurrentActStep
      pure $ ChaosTokenValue Skull $ NegativeModifier $ byDifficulty attrs ag (ac + ag)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | "swap the positions of your location and the location across from you",
guarded by "you are at an [[Adrift]] location".
-}
swapWithAcross :: ReverseQueue m => InvestigatorId -> m ()
swapWithAcross iid = do
  mlid <- selectOne $ locationWithInvestigator iid <> LocationWithTrait Adrift
  for_ mlid \lid -> getAcross lid >>= traverse_ (swapRingPositions lid)

-- | "draw the topmost treachery in the encounter discard pile"
drawTopmostTreacheryInDiscard :: ReverseQueue m => InvestigatorId -> m ()
drawTopmostTreacheryInDiscard iid = do
  encounterDiscard <- getEncounterDiscard RegularEncounterDeck
  for_ (find ((== TreacheryType) . toCardType) encounterDiscard) \card -> do
    obtainCard card
    push $ InvestigatorDrewEncounterCard iid card

instance RunMessage Unstuck where
  runMessage msg s@(Unstuck attrs) = runQueueT $ unstuckI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup Unstuck attrs do
      setup $ ul do
        li "gatherSets"
        li "setAside"
        li.nested "theRing" do
          li "removeOneVersion"
          li "shuffleAndPlace"
        li "shuffleElites"
        li.nested "checkCampaignLog" do
          li "steppedIntoThePast"
          li "steppedIntoTheFuture"
          li "fellToTheMyriad"
        li "addChaosToken"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.Unstuck
      gatherAndSetAside Set.Paradox

      setAside [Locations.theTimestream, Locations.arkhamMassachusetts_086]
      setAsideEveryCopyOf Treacheries.timeRunsBackwards

      kept <- for adriftVersions \versions -> do
        (inPlay, removed) <- sampleWithRest versions
        setAside removed
        pure inPlay

      shuffled <- shuffleM kept
      ring <- placeAllCapture shuffled
      let ringByDef = zip shuffled ring
      for_ (zip ring ringLabels) \(lid, lbl) -> push $ SetLocationLabel lid lbl
      for_ (zip ring (drop 1 ring <> take 1 ring)) \(l, r) ->
        push $ PlacedLocationDirection r RightOf l

      elites <-
        shuffle
          [ Enemies.determinedGeneral
          , Enemies.panzerIV
          , Enemies.shamblerFromTheStars
          , Enemies.tyrannosaurusRex
          ]
      setAsideFacedown (take 3 elites)

      setActDeck [Acts.allOfTimeAndSpace, Acts.fightingTheTide]
      setAgendaDeck
        [ Agendas.fallingApart
        , Agendas.daysNeverBeforeSeen
        , Agendas.badTimes
        , Agendas.breakingPoint
        ]

      pastward <- getHasRecord TheInvestigatorsSteppedIntoThePast
      futureward <- getHasRecord TheInvestigatorsSteppedIntoTheFuture
      if pastward
        then
          startAtOneOf
            ringByDef
            [Locations.arkhamMassachusetts_075, Locations.arkhamMassachusetts_076]
        else
          if futureward
            then
              startAtOneOf
                ringByDef
                [Locations.aDisquietingFuture_069, Locations.aDisquietingFuture_070]
            else do
              investigators <- allInvestigators
              scattered <- shuffleM ring
              for_ (zip investigators scattered) \(iid, lid) -> do
                reveal lid
                push $ PlaceInvestigator iid (AtLocation lid)

      addChaosToken case attrs.difficulty of
        Easy -> MinusTwo
        Standard -> MinusThree
        Hard -> MinusFour
        Expert -> MinusFive

      setMeta emptyUnstuckMeta
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      mtarget <- getSkillTestTarget
      for_ mtarget $ push . Msg.NotifyResultAgain . SkillTestInitiatorTarget
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Tablet -> do
      drawTopmostTreacheryInDiscard iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == ElderThing -> do
      withSkillTest \sid -> afterThisTestResolves sid $ swapWithAcross iid
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == ElderThing
      , isHardExpert attrs -> do
          withSkillTest \sid -> afterThisTestResolves sid $ swapWithAcross iid
          pure s
    ScenarioSpecific key v | key == registerRomanSoldierKey -> do
      let soldier = toResult @RomanSoldier v
      let meta = toResultDefault emptyUnstuckMeta attrs.meta
      pure $ Unstuck $ attrs & metaL .~ toJSON (UnstuckMeta $ soldier : meta.romanSoldiers)
    Msg.RemoveEnemy eid -> do
      let meta = toResultDefault emptyUnstuckMeta attrs.meta
      for_ (find (\c -> c.enemy == eid && not c.returned) meta.romanSoldiers) \soldier ->
        case soldier.card of
          PlayerCard pc -> do
            scenarioSpecific returnedRomanSoldierKey eid
            push $ Msg.AddToDiscard soldier.owner pc
          _ -> pure ()
      Unstuck <$> liftRunMessage msg attrs
    ScenarioSpecific key v | key == returnedRomanSoldierKey -> do
      let eid = toResult @EnemyId v
      let meta = toResultDefault emptyUnstuckMeta attrs.meta
      let mark c = if c.enemy == eid then c {romanSoldierReturned = True} else c
      pure $ Unstuck $ attrs & metaL .~ toJSON (UnstuckMeta $ map mark meta.romanSoldiers)
    Msg.Discarded (EnemyTarget eid) _ _ -> do
      let meta = toResultDefault emptyUnstuckMeta attrs.meta
      if any ((== eid) . (.enemy)) meta.romanSoldiers
        then pure s
        else Unstuck <$> liftRunMessage msg attrs
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> push R1
        Resolution 1 -> do
          investigatorDefeat
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> Unstuck <$> liftRunMessage msg attrs

startAtOneOf
  :: ReverseQueue m => [(CardDef, LocationId)] -> [CardDef] -> ScenarioBuilderT m ()
startAtOneOf ringByDef defs =
  case [lid | (def, lid) <- ringByDef, def `elem` defs] of
    lid : _ -> startAt lid
    [] -> traverse_ (startAt . snd) (listToMaybe ringByDef)

investigatorDefeat :: (HasI18n, ReverseQueue m) => m ()
investigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    resolutionOnly defeated $ withTitle "investigatorDefeat"
    copies <- getSetAsideCardsMatching $ cardIs Treacheries.timeRunsBackwards
    for_ (zip defeated copies) \(iid, card) ->
      addCampaignCardToDeck iid DoNotShuffleIn card
