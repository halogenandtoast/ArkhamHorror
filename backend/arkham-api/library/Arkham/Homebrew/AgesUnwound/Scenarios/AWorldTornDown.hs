{- | Scenario III. Can send the table back to itself (resolution 5) --
'Arkham.Homebrew.AgesUnwound.Campaign' takes that back-edge off the
@MustReplayAWorldTornDown@ record, which this scenario's 'Setup' crosses out.

Three entries are recorded /with a time/; see
'Arkham.Homebrew.AgesUnwound.Helpers.recordTheTimeFor'. Scenario VI's second act
deck advances off those records, so they must go through the @*For@ helpers and
never the @Text@ ones.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown (aWorldTornDown) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Helpers.Act (getActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (AnySkillValue))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Events
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Investigator.Types (Field (InvestigatorRemainingActions))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype AWorldTornDown = AWorldTornDown ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | No location grid, deliberately -- the school map lays itself out from its
connection symbols, as 'Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire' and
'Arkham.Homebrew.AgesUnwound.Scenarios.AYearToPlan' also do, and as Scenario VI
does for this same map.

A @grid-template-areas@ cell has to match a location's @locationLabel@
(@Scenario.vue@ hands each location @:gridArea="location.label"@), and this map
cannot supply one injectively:

* The three Classrooms all print symbol @T@ and are all named "Classroom", so
  they collide under 'symbolLabel' /and/ under the default
  @nameToLabel (cdName def)@. One cell cannot hold three simultaneous locations.
* Distinguishing them would mean this scenario pushing @SetLocationLabel@ for
  nine locations that belong to @night_of_the_ritual@, and Scenario VI --
  which places the same nine -- would have to duplicate every push or render a
  broken board. That is an unforgeable-by-nothing cross-scenario contract, the
  same shape of trap as the "record the time" tags.
* Only one of {Cafeteria, Principal's Office} / {three Classrooms} is ever in
  play, and only one Ritual Circle, so a fixed grid would leave several declared
  cells permanently empty -- which @unusedLabels@ renders as clickable
  placeholders.

Connections are 'Arkham.Matcher.LocationWithSymbol' off the def
(@Location/Types.hs:320@), independent of labels, so nothing is lost: Rear
Corridors' @T@ connection correctly reaches all three Classrooms, which is a
relationship a symbol-keyed grid could not express either.
-}

{- | Mount Hollyoke Elementary. All 15 printed connections join grid-adjacent
cells with no crossings: the grounds (circle/triangle/plus/square) along the
bottom, the interior above. Only one of the two entrances is ever in play, so
roughly half these cells sit empty in any given game.

The school locations are labelled by symbol, except the three Classrooms, which
all print @T@ and so carry explicit @classroom1@/@classroom2@/@classroom3@
labels set in their own modules -- that keeps the cell assignment with the cards
rather than as a push this scenario and Scenario VI would both have to repeat.
-}
aWorldTornDown :: Difficulty -> AWorldTornDown
aWorldTornDown difficulty =
  scenario
    AWorldTornDown
    ":ages-unwound:048"
    "A World Torn Down"
    difficulty
    [ "equals   hourglass heart  classroom3"
    , "moon     squiggle  classroom2 diamond"
    , "triangle plus      square classroom1"
    , ".        circle    .      ."
    ]

{- | "Set each other location aside, out of play" -- the Interior locations the
acts put into play, plus whichever Ritual Circle the branch calls for.
-}
setAsideLocations :: [CardDef]
setAsideLocations =
  [ Locations.ritualCircle_224
  , Locations.ritualCircle_225
  , Locations.cafeteria
  , Locations.principalsOffice
  , Locations.classroom_227
  , Locations.classroom_228
  , Locations.classroom_229
  ]

instance HasChaosTokenValue AWorldTornDown where
  getChaosTokenValue iid tokenFace (AWorldTornDown attrs) = case tokenFace of
    {- "-X. X is half the number of actions you have remaining (rounded up)" /
    "X is the number of actions you have remaining".

    "Actions you have remaining", not "standard actions": the campaign names the
    latter explicitly wherever it means it, so this is the plain remaining count. -}
    Skull -> do
      n <- field InvestigatorRemainingActions iid
      pure
        $ ChaosTokenValue Skull
        $ NegativeModifier
        $ byDifficulty attrs ((n + 1) `div` 2) n
    -- "-3/-5. If you succeed, you get +1 skill value during skill tests until the
    -- end of the round." The rider is the 'Msg.PassedSkillTest' case below.
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 5
    -- "-3/-5. If you fail, you get -1 skill value during skill tests until the
    -- end of the round." The rider is the 'Msg.FailedSkillTest' case below.
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    {- "Reveal another token. If you fail and it is your turn, end your turn." /
    "Reveal another token. If it is your turn, end your turn after resolving this
    skill test."

    The campaign's chaos bag holds no [elder_thing] until this scenario's own
    resolutions add one, so the effect can only ever fire on a replay -- which is
    exactly why this reference card prints it and Scenarios I and II do not. -}
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AWorldTornDown where
  runMessage msg s@(AWorldTornDown attrs) = runQueueT $ scenarioI18n $ case msg of
    {- "Check Campaign Log. If the investigators fled the Gentleman's manor:
    Proceed to Intro 1. If the investigators learnt of the Myriad's ritual: Skip
    to Intro 2." -}
    PreScenarioSetup -> scope "intro" do
      fled <- getHasRecord TheInvestigatorsFledTheGentlemansManor
      flavor $ setTitle "title" >> p (if fled then "intro1" else "intro2")
      pure s
    Setup -> runScenarioSetup AWorldTornDown attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "startAt"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      {- "Gather all the cards from the following encounter sets: A World Torn
      Down, Night of the Ritual, Agents of Aforgomon, Myriad, Nyctophobia, Thugs,
      Time Spirits, Unleashed Chaos."

      The guide's "Agents of Aforgomon" is the data's @agents_of_chronos@. "Time
      Spirits" is not an encounter set at all -- it is this campaign's Time Spirit
      enemy, which lives in @night_of_fire@ -- so only that card is gathered from
      there. Unleashed Chaos goes straight to the set-aside pool: setup step 3
      sets "each copy of Unleashed Chaos" aside, and /Backfire/ is what shuffles
      them in. -}
      gather Set.AWorldTornDown
      gather Set.NightOfTheRitual
      gather Set.AgentsOfChronos
      gather Set.Myriad
      gather Set.Nyctophobia
      gather Set.Thugs
      gatherJust Set.NightOfFire [Enemies.timeSpirit]
      gatherAndSetAside Set.UnleashedChaos

      setAgendaDeck [Agendas.theRitualBegins, Agendas.timeRunningOut, Agendas.endOfTheLine]

      {- Both stage-2 acts are in the deck; act 1b advances to whichever one its
      branch names and 'AdvanceToAct' drops the other, because it filters the
      remaining stack down to acts of a different stage. -}
      setActDeck
        [ Acts.theBoundary_052
        , Acts.bigAndUgly_053
        , Acts.doorwayToTheUnknown_054
        , Acts.breakingTheCircle
        ]

      {- "Put Front Gates, Side Building, Children's Playground, Sports Field,
      Front Hallway and Rear Corridors into play. Each investigator begins play at
      the Front Gates." -}
      placeAll
        [ Locations.sideBuilding_057
        , Locations.childrensPlayground_058
        , Locations.sportsField_059
        , Locations.frontHallway
        , Locations.rearCorridors
        ]
      startAt =<< place Locations.frontGates_056

      {- "Set each other location aside, out of play, along with Hound of
      Unmaking, Unstable Warding, each copy of Unleashed Chaos and Stirring Titan,
      Unstable Energies and The Myriad Gentleman (The High Priest)."

      /Aid from Afar/ is set aside too: the agenda's action draws "the set-aside
      Aid From Afar story card", and nothing else would put it out of play. -}
      setAside setAsideLocations
      setAside
        [ Enemies.houndOfUnmaking
        , Enemies.theMyriadGentleman_233
        , Assets.unstableWarding
        , Events.unstableEnergies
        , Stories.aidFromAfar
        ]
      setAsideEvery $ cardIs Treacheries.stirringTitan

      {- Resolution 5 sends the table back here; 'Arkham.Homebrew.AgesUnwound.Campaign'
      reads the uncrossed record, so crossing it out here is what stops the loop.
      'Msg.CrossOutRecord' only crosses out a key that is actually recorded, so
      this is a no-op on a first playthrough. -}
      crossOut MustReplayAWorldTornDown
    -- [cultist]: "If you succeed, you get +1 skill value during skill tests until
    -- the end of the round."
    Msg.PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      roundModifier Cultist iid (AnySkillValue 1)
      pure s
    -- [tablet]: "If you fail, you get -1 skill value during skill tests until the
    -- end of the round."
    Msg.FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Tablet -> do
      roundModifier Tablet iid (AnySkillValue (-1))
      pure s
    -- [elder thing]: "Reveal another token."
    ResolveChaosToken _ ElderThing iid -> do
      drawAnotherChaosToken iid
      {- Hard/expert: "If it is your turn, end your turn after resolving this
      skill test" -- unconditional on the result, unlike easy/standard below. -}
      when (isHardExpert attrs) do
        whenM (iid <=~> TurnInvestigator)
          $ withSkillTest \sid -> afterThisTestResolves sid $ endYourTurn iid
      pure s
    -- [elder thing], easy/standard: "If you fail and it is your turn, end your turn."
    Msg.FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == ElderThing
      , isEasyStandard attrs -> do
          whenM (iid <=~> TurnInvestigator) $ endYourTurn iid
          pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        {- "If no resolution was reached (each investigator was defeated) and it
        was act 1 when the scenario ended: Read Resolution 1. ... and it was act 2
        or 3 ...: Read Resolution 2." -}
        NoResolution -> do
          step <- fromMaybe 1 <$> (traverse getActStep =<< selectOne AnyAct)
          push $ if step == 1 then R1 else R2
        {- Resolution 1: "Check Campaign Log: If the timeline was weakened,
        proceed to Resolution 4. Otherwise, proceed to Resolution 5." -}
        Resolution 1 -> do
          resolution "resolution1"
          weakened <- getHasRecord TheTimelineWasWeakened
          push $ if weakened then R4 else R5
        Resolution 2 -> do
          {- "record that the investigators fell to the Myriad. Next to this,
          record the time. If there was no agenda in play, record the time as
          (3,4)." -- which is what 'getTheTime' falls back to. -}
          record TheInvestigatorsFellToTheMyriad
          recordTheTimeFor TheInvestigatorsFellToTheMyriad
          recordHighPriestEliminated

          -- "Remove all [cultist] and [tablet] tokens from the chaos bag. Then,
          -- add 1 [cultist], 1 [tablet] and 2 [elder thing] tokens."
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          addChaosToken Cultist
          addChaosToken Tablet
          twice $ addChaosToken ElderThing

          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        Resolution 3 -> do
          {- "If Ritual Circle (Gateway to the Past) is in play: ... stepped into
          the past ... add 2 [tablet] and 1 [elder thing]. /
          If Ritual Circle (A Harnessed Future) is in play: ... stepped into the
          future ... add 2 [cultist] and 1 [elder thing]."

          Exactly one is in play: act 2b /Physical Education/ (the front door)
          puts Gateway to the Past down, act 2b /Myriad Encounters/ (the back
          door) A Harnessed Future. -}
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          gateway <- selectAny $ locationIs Locations.ritualCircle_225
          if gateway
            then do
              record TheInvestigatorsSteppedIntoThePast
              twice $ addChaosToken Tablet
            else do
              record TheInvestigatorsSteppedIntoTheFuture
              twice $ addChaosToken Cultist
          addChaosToken ElderThing

          recordHighPriestEliminated
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        Resolution 4 -> do
          -- "record that all times are one. Each investigator is driven insane.
          -- The investigators lose the campaign."
          record AllTimesAreOne
          resolution "resolution4"
          eachInvestigator drivenInsane
          gameOver
        Resolution 5 -> do
          {- "record that the timeline was weakened. Add 2 [elder thing] tokens to
          the chaos bag. The investigators must replay Scenario III ... No
          experience points are earned from your previous game." -- hence
          'resolution' rather than 'resolutionWithXp', and the campaign's
          'continueNoUpgrade' back-edge. -}
          record TheTimelineWasWeakened
          twice $ addChaosToken ElderThing
          record MustReplayAWorldTornDown
          resolution "resolution5"
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> AWorldTornDown <$> liftRunMessage msg attrs

{- | "If The Myriad Gentleman (The High Priest) is in the victory display, record
in your Campaign Log that the investigators eliminated the high priest."
Resolutions 2 and 3 both do it.
-}
recordHighPriestEliminated :: ReverseQueue m => m ()
recordHighPriestEliminated = do
  eliminated <-
    selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.theMyriadGentleman_233
  when eliminated $ record TheInvestigatorsEliminatedTheHighPriest
