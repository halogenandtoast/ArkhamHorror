{- | Scenario VII, the finale. Can send the table back to itself (resolution 3) --
'Arkham.Homebrew.AgesUnwound.Campaign' takes that back-edge off the
@MustReplayTimeRunsOut@ record, which this scenario's 'Setup' crosses out.

Its nineteen locations come in v1/v2 printings of the same map slot; each pair
shares a badge and a connection arc, and most of the backs are a card of their
own (an enemy, a story or a treachery). Setup keeps one printing of each pair at
random and sets the other aside; everything the pairs, the wards and the
beneath-the-agenda-deck pile need lives in
"Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers".
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut (timeRunsOut) where

import Arkham.Card
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Id
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype TimeRunsOut = TimeRunsOut ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The eleven map slots, by badge, laid out as three eras: the @[[Past]]@ on the
left (Dawn of the Universe, Secrets Long Forgotten, Days That Never Were, The
Past), the @[[Present]]@ down the middle (Today a Thousand Times, Fulcrum of
Possibility, The Present) and the @[[Future]]@ on the right (What Could Be, What
Could Never Be, The End of All Things, The Future).

The cells are named by location symbol, which is what 'symbolLabel' writes on
every printing of a slot -- so whichever of a pair setup keeps, and whichever
@[[Paradox]]@ version act 4 later puts into play, lands in the same cell.
-}

{- | The expanse outside time. This map is dense -- 24 printed connections over
11 locations, including two K4 cliques (Days That Never Were / Secrets Long
Forgotten / The Past / Dawn of the Universe, and What Could Be / What Could
Never Be / The Future / The End of All Things) joined through the Fulcrum of
Possibility hub -- so a fully planar grid is not available. Each clique sits in
its own 2x2 block with the hub between them, which is the best found: 3 of the
24 connections span more than one cell and 3 pairs of lines cross, down from 4
and 11. Each location carries 'symbolLabel', so its cell is its symbol, and only
one printing of each paired location is ever in play.
-}
timeRunsOut :: Difficulty -> TimeRunsOut
timeRunsOut difficulty =
  scenario
    TimeRunsOut
    ":ages-unwound:182"
    "Time Runs Out"
    difficulty
    [ "star .       .      .        ."
    , "moon diamond plus   heart    ."
    , ".    t       circle triangle equals"
    , ".    .       square hourglass ."
    ]

{- | Scenario reference card, @:ages-unwound:182@:

Easy / Standard
[skull]: -X. X is the current act number.
[cultist]: -2. If you succeed and it is your turn, gain an action.
[tablet]: -4. If you fail and it is your turn, lose 1 action.
[elder_thing]: -5. If you fail, either exile a card from your hand or place 1 doom on your location.

Hard / Expert
[skull]: -X. X is twice the current act number.
[cultist]: -3. If you succeed and it is your turn, gain an action.
[tablet]: -5. It is your turn and you did not succeed by at least 2, lose 1 action.
[elder_thing]: -8. If you fail, either exile a card from your hand or place 1 doom on your location.

Note the [tablet] rider widens on Hard and Expert: there it also fires on a
success by fewer than 2, not only on a failure.
-}
instance HasChaosTokenValue TimeRunsOut where
  getChaosTokenValue iid tokenFace (TimeRunsOut attrs) = case tokenFace of
    Skull -> do
      n <- getCurrentActStep
      pure $ ChaosTokenValue Skull $ NegativeModifier $ byDifficulty attrs n (n * 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 5 8
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TimeRunsOut where
  runMessage msg s@(TimeRunsOut attrs) = runQueueT $ timeRunsOutI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      pure s
    Setup -> runScenarioSetup TimeRunsOut attrs do
      setup $ ul do
        li "gatherSets"
        li "checkCampaignLog"
        li.nested "placeLocations" do
          li "removeOtherVersions"
          li "startAt"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      {- "Gather all cards from the following encounter sets: Time Runs Out,
      Agents of Aforgomon, Paradox, Unravelling Years." The guide's "Agents of
      Aforgomon" is the data's @agents_of_chronos@ and its "Unravelling Ages" the
      data's @unravelling_years@. -}
      gather Set.TimeRunsOut
      gather Set.AgentsOfChronos
      gather Set.Paradox
      gather Set.UnravellingYears

      -- "Check Campaign Log. If the investigators unleashed chaos, also gather
      -- the Unleashed Chaos encounter set."
      unleashedChaos <- getHasRecord TheInvestigatorsUnleashedChaos
      when unleashedChaos $ gather Set.UnleashedChaos

      setAgendaDeck [Agendas.outsideOfTime, Agendas.familiarAdversaries]
      setActDeck
        [ Acts.outOfYourDepth
        , Acts.preservation
        , Acts.againstAGod
        , Acts.weatheringTheStorm
        ]

      {- "Put one of the two versions of the following locations into play at
      random, revealed side faceup ... Remove the other versions of each of those
      locations from the game. Each investigator begins play in Fulcrum of
      Possibility."

      The loser of each pair goes to the set-aside pool rather than being dropped:
      "removed from the game" is the guide's randomiser for "choose a random
      location" (see 'getRandomLocation'), and keeping the cards is what lets a
      later effect name a printing at all. -}
      kept <- for locationVersions \versions -> do
        (inPlay, removed) <- sampleWithRest versions
        setAside removed
        pure inPlay

      placed <- placeAllCapture kept
      {- The defs and the ids 'placeAllCapture' hands back line up, and that
      pairing is the only way to know which printing of a named place ended up in
      play: the locations do not exist in game state until the queued
      PlaceLocation messages run, so a `select` here would find nothing. -}
      startAtOneOf
        (zip kept placed)
        [Locations.fulcrumOfPossibility_189, Locations.fulcrumOfPossibility_190]

      -- "Set The Past, The Present, The Future, and Yourself aside, out of play."
      setAside
        [ Locations.thePast
        , Locations.thePresent
        , Locations.theFuture
        , Enemies.yourself
        ]

      {- Resolution 3 sends the table back here;
      'Arkham.Homebrew.AgesUnwound.Campaign' reads the uncrossed record, so
      crossing it out here is what stops the loop. @CrossOutRecord@ only crosses
      out a key that is actually recorded, so this is a no-op on a first run. -}
      crossOut MustReplayTimeRunsOut

      -- The scenario is the one place that remembers which player card each copy
      -- of Yourself was made from; see the handlers below.
      setMeta emptyTimeRunsOutMeta
    -- [cultist]: "If you succeed and it is your turn, gain an action."
    Msg.PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      whenM (iid <=~> TurnInvestigator) $ gainActions iid Cultist 1
      pure s
    -- [tablet], easy/standard: "If you fail and it is your turn, lose 1 action."
    Msg.FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Tablet
      , isEasyStandard attrs -> do
          whenM (iid <=~> TurnInvestigator) $ loseStandardActions iid Tablet 1
          pure s
    {- [tablet], hard/expert: "It is your turn and you did not succeed by at least
    2, lose 1 action." Failing is one way not to succeed by 2, so the rider has
    both a failure and a narrow-success branch. -}
    Msg.FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Tablet
      , isHardExpert attrs -> do
          whenM (iid <=~> TurnInvestigator) $ loseStandardActions iid Tablet 1
          pure s
    Msg.PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n
      | token.face == Tablet
      , isHardExpert attrs
      , n < 2 -> do
          whenM (iid <=~> TurnInvestigator) $ loseStandardActions iid Tablet 1
          pure s
    -- [elder thing]: "If you fail, either exile a card from your hand or place 1
    -- doom on your location."
    Msg.FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == ElderThing -> do
      hand <- select $ inHandOf NotForPlay iid <> basic NonWeakness
      lid <- getJustLocation iid
      chooseOrRunOneM iid $ scope "elderThing" do
        labeledValidate (notNull hand) "exileACard" $ chooseAndExileFromHand iid
        labeled "placeDoomOnYourLocation" $ placeDoom ElderThing lid 1
      pure s
    {- Act 2b mints a copy of Yourself onto the top card of each investigator's
    deck. The scenario owns the bookkeeping, because once the @:ages-unwound:208@
    def is swapped in nothing on the enemy knows which player card it stood in
    for. Registration is a 'Msg.ScenarioSpecific' message rather than a
    @SetScenarioMeta@ read-modify-push, so four copies spawned in one handler
    cannot clobber each other. -}
    Msg.ScenarioSpecific key v | key == registerYourselfKey -> do
      let copy = toResult @YourselfCopy v
      let meta = toResultDefault emptyTimeRunsOutMeta attrs.meta
      pure $ TimeRunsOut $ attrs & metaL .~ toJSON (TimeRunsOutMeta $ copy : meta.yourselves)
    {- The act prints no return path, so the ordinary rule governs: a player card
    put into play goes to its owner's discard pile when it leaves play.
    'Msg.RemoveEnemy' is the single point every leave-play path funnels through. -}
    Msg.RemoveEnemy eid -> do
      let meta = toResultDefault emptyTimeRunsOutMeta attrs.meta
      for_ (find (\c -> c.enemy == eid && not c.returned) meta.yourselves) \copy ->
        case copy.card of
          PlayerCard pc -> do
            scenarioSpecific returnedYourselfKey eid
            push $ Msg.AddToDiscard copy.owner pc
          _ -> pure ()
      TimeRunsOut <$> liftRunMessage msg attrs
    Msg.ScenarioSpecific key v | key == returnedYourselfKey -> do
      let eid = toResult @EnemyId v
      let meta = toResultDefault emptyTimeRunsOutMeta attrs.meta
      let mark c = if c.enemy == eid then c {yourselfCopyReturned = True} else c
      pure $ TimeRunsOut $ attrs & metaL .~ toJSON (TimeRunsOutMeta $ map mark meta.yourselves)
    {- A copy of Yourself is an /encounter/ card standing in for a player card, so
    the default bookkeeping would file it in the encounter discard pile -- where it
    could then be drawn as an encounter card. Swallow the message;
    'Msg.RemoveEnemy' above has already sent the real card home. -}
    Msg.Discarded (EnemyTarget eid) _ _ -> do
      let meta = toResultDefault emptyTimeRunsOutMeta attrs.meta
      if any ((== eid) . (.enemy)) meta.yourselves
        then pure s
        else TimeRunsOut <$> liftRunMessage msg attrs
    ScenarioResolution res -> scope "resolutions" do
      case res of
        -- "If no resolution was reached (each investigator was defeated): Read
        -- Resolution 1."
        NoResolution -> push R1
        Resolution 1 -> do
          -- "record that all times are one. Each investigator is driven insane.
          -- The investigators lose the campaign."
          record AllTimesAreOne
          resolution "resolution1"
          eachInvestigator drivenInsane
          gameOver
        Resolution 2 -> do
          {- "record that the investigators bound Aforgomon in a prison of time. /
          For each investigator who ended the game with at least 1 copy of the
          Unspeakable Oath weakness in their hand, record in your Campaign Log
          that '[investigator name] still bears Aforgomon's mark'. /
          In your Campaign Log, record the names of each investigator whose
          'existence is waning'. / The investigators win the campaign!"

          The Unspeakable Oath is matched by title, because it is three official
          basic weaknesses (Bloodthirst, Curiosity, Cowardice) that share one name
          -- and all three are [[Hidden]], which is why "in their hand" is where
          the game ends with them.

          The waning entries are already in each investigator's log: /Grandfather
          Paradox/ and /Unwritten Existence/ record them as they happen, because
          cards in this scenario read the entry back mid-game. Re-recording here is
          idempotent and is what makes the resolution's own step real rather than
          implied. -}
          record TheInvestigatorsBoundAforgomonInAPrisonOfTime

          bearers <- select $ HandWith (HasCard $ CardWithTitle "Unspeakable Oath")
          for_ bearers (`recordForInvestigator` StillBearsAforgomonsMark)

          waning <- select $ investigatorWithRecord ExistenceIsWaning
          for_ waning (`recordForInvestigator` ExistenceIsWaning)

          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        Resolution 3 -> do
          {- "Add 1 [elder_thing] token to the chaos bag. If you cannot, each
          investigator takes 1 mental trauma instead. /
          The investigators must replay Scenario VII ... Do not record anything
          else in your Campaign Log except for any trauma suffered from your
          previous game. No experience points are earned from your previous
          game."

          Hence 'resolution' rather than 'resolutionWithXp', and the campaign's
          'continueNoUpgrade' back-edge off 'MustReplayTimeRunsOut'. "If you
          cannot" is the box's four [elder_thing] tokens -- see
          'elderThingSupply', which documents why the cap is modelled rather than
          ignored. -}
          canAdd <- canAddElderThing
          if canAdd
            then addChaosToken ElderThing
            else eachInvestigator (`sufferMentalTrauma` 1)

          record MustReplayTimeRunsOut
          resolution "resolution3"
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> TimeRunsOut <$> liftRunMessage msg attrs

{- | "Each investigator begins play in Fulcrum of Possibility" -- whichever
printing setup kept.
-}
startAtOneOf
  :: ReverseQueue m => [(CardDef, LocationId)] -> [CardDef] -> ScenarioBuilderT m ()
startAtOneOf byDef defs =
  case [lid | (def, lid) <- byDef, def `elem` defs] of
    lid : _ -> startAt lid
    -- Unreachable: one printing of every pair is always placed. Falling back
    -- keeps setup from leaving the investigators nowhere if it ever is not.
    [] -> traverse_ (startAt . snd) (listToMaybe byDef)
