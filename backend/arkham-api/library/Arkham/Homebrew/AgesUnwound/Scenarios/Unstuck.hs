{- | Scenario IV. The investigators fall out of time and tumble through eras.

The board is a __ring__: one printing of each of the eight repeated locations is
kept at random and the other removed from the game, then the eight survivors are
shuffled and arranged into a circle. Carnevale of Horrors is the precedent for
the arrangement -- grid cells named by label, bound to the shuffled cards with
'SetLocationLabel', chained with 'PlacedLocationDirection' so that clockwise is
@RightOf@. Everything the ring needs afterwards lives in
"Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers".

The starting location branches on Scenario III's outcome, and setup adds a
difficulty-scaled negative token to the chaos bag for the rest of the campaign.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck (unstuck) where

import Arkham.Card
import Arkham.Direction
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.FlavorText (flavor, h, li, p, resolutionOnly, setup, ul, withTitle)
import Arkham.Helpers.Query (allInvestigators, getSetAsideCardsMatching)
import Arkham.Helpers.SkillTest (withSkillTest)
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

{- | The circle, clockwise from the top, with The Timestream and Arkham,
Massachusetts in the middle -- they arrive from the set-aside pool when act 1
advances and take the two centre cells.
-}
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

{- | "Based on your difficulty level, add the following chaos token to the chaos
bag for the remainder of the campaign."
-}
campaignToken :: Difficulty -> ChaosTokenFace
campaignToken = \case
  Easy -> MinusTwo
  Standard -> MinusThree
  Hard -> MinusFour
  Expert -> MinusFive

{- | Scenario reference card, @:ages-unwound:062@:

Easy / Standard
[skull]: -X. X is the current agenda number.
[cultist]: Reveal another token. If you fail, resolve the effects of the failed test an additional time.
[tablet]: -2. If you fail, draw the topmost treachery in the encounter discard pile.
[elder_thing]: -4. If you fail and you are at an [[Adrift]] location, after this test ends, swap the positions of your location and the location across from you.

Hard / Expert
[skull]: -X. X is the sum of the current act number and the current agenda number.
[cultist]: Reveal another token. If you fail, resolve the effects of the failed test an additional time.
[tablet]: -3. If you fail, draw the topmost treachery in the encounter discard pile.
[elder_thing]: -5. If you are at an [[Adrift]] location, after this test ends, swap the positions of your location and the location across from you.

Note the [elder thing] rider loses its "if you fail" on Hard and Expert: there
the swap happens whether the test passed or failed.
-}
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

      -- "Gather all cards from the Unstuck and Paradox encounter sets." The
      -- Paradox set is gathered straight to the set-aside pool; agenda 2b
      -- shuffles it in.
      gather Set.Unstuck
      gatherAndSetAside Set.Paradox

      {- "Set the Paradox encounter set, The Timestream and Arkham, Massachusetts
      (Present Day?) locations, and each copy of the Time Runs Backwards
      weakness aside, out of play." -}
      setAside [Locations.theTimestream, Locations.arkhamMassachusetts_086]
      setAsideEvery $ cardIs Treacheries.timeRunsBackwards

      {- "Remove one of the two copies of each other location from the game, at
      random. Then shuffle the eight remaining locations together and put them
      into play in a random order, forming a circle."

      The removed copies are not really gone: agenda 3b swaps a location for
      "the version that was removed from the game", so they go to the set-aside
      pool, which is where that swap looks for them. (The guide's other use of
      the removed pile -- drawing one at random to pick a random [[Adrift]]
      location -- needs no cards at all: see 'getRandomAdriftLocation'.) -}
      kept <- for adriftVersions \versions -> do
        (inPlay, removed) <- sampleWithRest versions
        setAside removed
        pure inPlay

      shuffled <- shuffleM kept
      ring <- placeAllCapture shuffled
      {- The shuffled cards and the ids 'placeAllCapture' hands back line up, and
      that pairing is the only way to know which printing of a named place ended
      up in play: the locations do not exist in game state until the queued
      PlaceLocation messages run, so a `select` here would find nothing. -}
      let ringByDef = zip shuffled ring
      for_ (zip ring ringLabels) \(lid, lbl) -> push $ SetLocationLabel lid lbl
      for_ (zip ring (drop 1 ring <> take 1 ring)) \(l, r) ->
        push $ PlacedLocationDirection r RightOf l

      {- "Shuffle the four Elite enemies together. Set three of them aside, out
      of play, without looking at them. (The fourth will be shuffled into the
      encounter deck.)" Set-aside face down, so the client shows backs; Arkham,
      Massachusetts picks from them at random when it is revealed. -}
      elites <-
        shuffleM
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

      {- "Check Campaign Log. If the investigators stepped into the past, each
      investigator begins play at Arkham, Massachusetts (16th Century). If the
      investigators stepped into the future, each investigator begins play at A
      Disquieting Future. If the investigators fell to the Myriad, each
      investigator begins play at a different random [[Adrift]] location."

      Only one printing of each named place is in play, so the lookup names both
      and the ring decides which exists. With none of the three recorded (a
      standalone table) the third branch is the fallback: it needs no named
      location. -}
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

      -- "Based on your difficulty level, add the following chaos token to the
      -- chaos bag for the remainder of the campaign."
      push $ AddChaosToken (campaignToken attrs.difficulty)

      -- The scenario is the one place that remembers which player card each
      -- Roman Soldier was made from; see the handlers below.
      setMeta emptyUnstuckMeta

    {- [cultist]: "Reveal another token. If you fail, resolve the effects of the
    failed test an additional time."

    TODO(ages-unwound): only the reveal-another half is implemented. There is no
    engine primitive for "resolve the effects of the failed test an additional
    time" -- the failure effects of a test are whatever the test's own source
    queued, and nothing records them as a re-runnable unit. Doing it properly
    needs a general "repeat this test's failure resolution" seam (the closest
    existing thing, 'RerunSkillTest', re-runs the whole test rather than its
    effects), which is a core change and is reported rather than invented here. -}
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    {- [tablet]: "If you fail, draw the topmost treachery in the encounter
    discard pile." -}
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Tablet -> do
      drawTopmostTreacheryInDiscard iid
      pure s
    {- [elder thing]: "If you fail and you are at an [[Adrift]] location, after
    this test ends, swap the positions of your location and the location across
    from you." Easy/Standard only -- on Hard/Expert the swap is not conditional
    on failing, which is the 'PassedSkillTest' case below. -}
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == ElderThing -> do
      withSkillTest \sid -> afterThisTestResolves sid $ swapWithAcross iid
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == ElderThing
      , isHardExpert attrs -> do
          withSkillTest \sid -> afterThisTestResolves sid $ swapWithAcross iid
          pure s
    {- /Determined General/ and /Roman Outpost/ put the top card of an
    investigator's deck into play as a Roman Soldier. The scenario owns the
    bookkeeping, because once the @:ages-unwound:900@ def is swapped in nothing
    on the enemy knows which player card it stood in for. Registration is a
    'ScenarioSpecific' message rather than a @SetScenarioMeta@
    read-modify-push, so two soldiers spawned in one handler cannot clobber each
    other. -}
    ScenarioSpecific key v | key == registerRomanSoldierKey -> do
      let soldier = toResult @RomanSoldier v
      let meta = toResultDefault emptyUnstuckMeta attrs.meta
      pure $ Unstuck $ attrs & metaL .~ toJSON (UnstuckMeta $ soldier : meta.romanSoldiers)
    {- Neither card prints a return path, so the ordinary rule governs: a player
    card put into play goes to its owner's discard pile when it leaves play.
    'RemoveEnemy' is the single point every leave-play path funnels through
    (defeat, discard, removal), and the scenario runs before
    'Arkham.Game.Runner' handles it. -}
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
    {- A soldier's card is an /encounter/ card standing in for a player card, so
    the default bookkeeping would file it in the encounter discard pile -- where
    it could then be drawn as an encounter card. Swallow the message;
    'RemoveEnemy' above has already sent the real card home. -}
    Msg.Discarded (EnemyTarget eid) _ _ -> do
      let meta = toResultDefault emptyUnstuckMeta attrs.meta
      if any ((== eid) . (.enemy)) meta.romanSoldiers
        then pure s
        else Unstuck <$> liftRunMessage msg attrs
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "Before resolving any other resolution, if at least one investigator
        was defeated: The defeated investigators read Investigator Defeat first."
        Reaching no resolution at all (agenda 4b defeats everyone) still gets
        there, because Resolution 1 is the only resolution this scenario has. -}
        NoResolution -> push R1
        Resolution 1 -> do
          investigatorDefeat
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> Unstuck <$> liftRunMessage msg attrs

{- | "each investigator begins play at <named location>" -- whichever printing of
that place the ring kept.
-}
startAtOneOf
  :: ReverseQueue m => [(CardDef, LocationId)] -> [CardDef] -> ScenarioBuilderT m ()
startAtOneOf ringByDef defs =
  case [lid | (def, lid) <- ringByDef, def `elem` defs] of
    lid : _ -> startAt lid
    -- Unreachable: one printing of every pair is always in the ring. Falling
    -- back keeps setup from leaving the investigators nowhere if it ever is not.
    [] -> traverse_ (startAt . snd) (listToMaybe ringByDef)

{- | "Investigator Defeat: ... Each investigator who was defeated gains the Time
Runs Backwards weakness, and must add it to their deck. Go to Resolution 1."

Read before any other resolution and only by the investigators it names, which
is what 'resolutionOnly' is for.
-}
investigatorDefeat :: (HasI18n, ReverseQueue m) => m ()
investigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    resolutionOnly defeated $ withTitle "investigatorDefeat"
    {- One *distinct* set-aside copy per investigator -- do not "simplify" this
    back to @for_ defeated (\iid -> addCampaignCardToDeck iid _ def)@.

    Passing the def would hand every defeated investigator the same card id.
    @FetchCard CardDef@ prefers the set-aside pool over minting
    (@Helpers/FetchCard.hs@: @maybe (Just <$> genCard def) (pure . Just) =<<
    maybeGetSetAsideCard def@), and setup set all four copies aside, so the
    set-aside branch wins; @maybeGetSetAsideCard@ only *matches* a copy and the
    @AddCampaignCardToDeck@ handler only @replaceCard@s it -- neither consumes
    the pool -- so every iteration would fetch the same card and re-own it, and
    the last writer would win. The def-passing idiom is fine for a card that was
    never set aside, where @genCard@ mints a fresh one per call.

    Four printed copies and at most four investigators, so the pairing is exact;
    'zip' is safe regardless. -}
    copies <- getSetAsideCardsMatching $ cardIs Treacheries.timeRunsBackwards
    for_ (zip defeated copies) \(iid, card) ->
      addCampaignCardToDeck iid DoNotShuffleIn card
