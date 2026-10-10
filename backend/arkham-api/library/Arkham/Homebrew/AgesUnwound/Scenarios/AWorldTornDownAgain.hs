{- | Scenario VI. Two act decks: @a/b@ in the present (deck 1) and @c/d@ tracking
the investigators' past selves (deck 2).

__Deck 2 is load-bearing.__ @night_of_the_ritual@'s /Backfire/ advances
@selectOne (ActWithDeckId 1)@ by hand, deliberately avoiding
@advanceCurrentAct@/@advanceTheAct@, because three helpers @error@ the moment a
second act deck exists: @Scenario.Types.scenarioActs@
(so @RemainingActMatcher@ reads are unavailable through
@Game.getRemainingActsMatching@) and @Helpers.Act.getCurrentActStep@. Nothing in
this scenario or in any card it can put into play reaches them; the past deck is
read with
'Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers.getActStepInDeck'.

Investigators who /returned to Arkham late/ begin at no location and out of play
until the end of the first round; see 'theLateSeats' and 'outOfPlaySeats' below
for the four loops that ignore placement and how each is handled.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain (aWorldTornDownAgain) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  modifySelect,
  modifySelectWith,
  setActiveDuringSetup,
 )
import Arkham.Helpers.Query (getActiveInvestigatorId, getInvestigators)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Events
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (InvestigatorRemainingActions))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioTurn))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype AWorldTornDownAgain = AWorldTornDownAgain ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The school revisited. Same map as Scenario III plus this scenario's own
Ritual Circle (@star@); all 17 printed connections join grid-adjacent cells with
no crossings. Featureless Streets prints no symbol and no connections, so it is
deliberately not given a cell -- it is only ever in play on its own.

As in Scenario III the three Classrooms carry explicit
@classroom1@/@classroom2@/@classroom3@ labels from their own modules, because
all three print @T@.
-}
aWorldTornDownAgain :: Difficulty -> AWorldTornDownAgain
aWorldTornDownAgain difficulty =
  scenarioWith
    AWorldTornDownAgain
    ":ages-unwound:155"
    "A World Torn Down, Again"
    difficulty
    [ "classroom3 classroom1 circle   ."
    , "diamond    square     plus     triangle"
    , "classroom2 heart      squiggle equals"
    , ".          star       hourglass moon"
    ]
    -- Two act decks flanking the agenda deck.
    (decksLayoutL .~ ["act1 agenda1 act2"])

{- | "Based on your difficulty level, add the following chaos token to the chaos
bag for the remainder of the campaign."
-}
campaignToken :: Difficulty -> ChaosTokenFace
campaignToken = \case
  Easy -> MinusThree
  Standard -> MinusFive
  Hard -> MinusSix
  Expert -> MinusSeven

{- | The investigators who "returned to Arkham late" and have not yet entered
play.

Scenario V records 'ReturnedToArkhamLate' per investigator. The placement half of
the conjunction is what makes this stop matching the moment they enter play at
the end of the first round.
-}
theLateSeats :: InvestigatorMatcher
theLateSeats = investigatorWithRecord ReturnedToArkhamLate <> InvestigatorWithPlacement Unplaced

{- | Every investigator who is out of play, whatever put them there.

Broader than 'theLateSeats' on purpose: Ritual Circle /A Harnessed Future/
(@:ages-unwound:224@, put into play by act 3a) unplaces an investigator for its
haunted and returns them at the next investigation phase, and the two loops a
modifier cannot reach --- the turn offer and the mythos encounter draw --- are just
as wrong for them. Only the end-of-round /entry/ is kept narrow, since a
suspended investigator must come back to their own circle, not to the starting
location.
-}
outOfPlaySeats :: InvestigatorMatcher
outOfPlaySeats = InvestigatorWithPlacement Unplaced

{- | The @night_of_the_ritual@ locations the acts put into play, plus Scenario
VI's own Ritual Circle. The two @night_of_the_ritual@ Ritual Circles are added
conditionally in 'Setup' --- whichever one the investigators stepped into a year
ago is removed from the game.
-}
sharedSetAsideLocations :: [CardDef]
sharedSetAsideLocations =
  [ Locations.ritualCircle_173
  , Locations.cafeteria
  , Locations.principalsOffice
  , Locations.classroom_227
  , Locations.classroom_228
  , Locations.classroom_229
  ]

instance HasChaosTokenValue AWorldTornDownAgain where
  getChaosTokenValue iid tokenFace (AWorldTornDownAgain attrs) = case tokenFace of
    {- "-X. X is the number of actions you have remaining" / "X is 1 more than the
    number of actions you have remaining".

    "Actions you have remaining", not "standard actions": this campaign names the
    latter explicitly wherever it means it. -}
    Skull -> do
      n <- field InvestigatorRemainingActions iid
      pure $ ChaosTokenValue Skull $ NegativeModifier $ byDifficulty attrs n (n + 1)
    -- "-1/-3. If you succeed, you get +1 skill value during skill tests until the
    -- end of the round." The rider is the 'Msg.PassedSkillTest' case below.
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    -- "-3/-5. If you fail, place 1 doom on the current agenda." The rider is the
    -- 'Msg.FailedSkillTest' case below.
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    -- "-5. Take 1 horror." / "You automatically fail. Take 1 horror." The horror
    -- is unconditional on both sides; see 'ResolveChaosToken' below.
    ElderThing ->
      pure
        $ ChaosTokenValue ElderThing
        $ byDifficulty attrs (NegativeModifier 5) AutoFailModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance HasModifiersFor AWorldTornDownAgain where
  getModifiersFor (AWorldTornDownAgain a) = do
    {- "If you solved the riddle of the sphinx, each investigator begins the game
    with 1 additional card and 2 additional resources."

    Opening hands are drawn before 'Setup' runs (@Scenario/Runner.hs@ queues
    @SetupInvestigators@/@InvestigatorsMulligan@ ahead of it), so this cannot be a
    setup instruction -- it has to be a modifier that is already live, and
    'setActiveDuringSetup' is what lets it be read with no active investigator. -}
    whenM (getHasRecord YouSolvedTheRiddleOfTheSphinx) do
      modifySelectWith a Anyone setActiveDuringSetup [StartingHand 1, StartingResources 2]

    {- "Each investigator who returned to Arkham late performs setup normally, but
    begins the game at no location. They are not considered to be in play, and
    cannot interact with the game in any way."

    Gated on the first round for two reasons: the seats are placed at the end of
    it, and *every* investigator is 'Unplaced' until setup places them --
    @Helpers/Investigator.hs:420@ -- so an ungated 'CannotDrawCards' would eat
    everyone's opening hand.

    These modifiers cover three of the four loops that iterate investigators
    without checking placement. The fourth, the mythos encounter draw, cannot be
    reached by a modifier and is handled at the 'Msg.CheckWindows' case below;
    the investigation-phase turn offer is handled at 'Msg.BeginTurn'. -}
    whenM (scenarioFieldMap ScenarioTurn (== 1)) do
      modifySelect
        a
        theLateSeats
        [ CannotDrawCards
        , CannotGainResources
        , CannotTakeAction IsAnyAction
        , CannotPlay AnyCard
        , CannotMove
        , CannotBeAttacked
        , CannotBeEngaged
        ]

instance RunMessage AWorldTornDownAgain where
  runMessage msg s@(AWorldTornDownAgain attrs) = runQueueT $ scenarioI18n $ case msg of
    {- "Check Campaign Log. If your hunters found a new quarry: Proceed to Intro 1
    ... If the Myriad harnessed the power of another realm: Skip to Intro 2 ... If
    neither of the above statements are true: Skip to Intro 3."

    Intro 1 chains into the same check, so a table with both records reads 1 then
    2, a table with only the quarry reads 1 then 3. -}
    PreScenarioSetup -> scope "intro" do
      quarry <- getHasRecord YourHuntersFoundANewQuarry
      harnessed <- getHasRecord TheMyriadHarnessedThePowerOfAnotherRealm
      flavor do
        setTitle "title"
        when quarry $ p "intro1"
        p (if harnessed then "intro2" else "intro3")
      -- Intro 1: "Each investigator takes 1 direct damage and 1 direct horror."
      when quarry $ eachInvestigator \iid -> directDamageAndHorror iid attrs 1 1
      pure s
    Setup -> runScenarioSetup AWorldTornDownAgain attrs do
      violatedCausality <- getHasRecord TheInvestigatorsViolatedCausality
      harnessed <- getHasRecord TheMyriadHarnessedThePowerOfAnotherRealm
      frontDoor <- getHasRecord TheInvestigatorsUsedTheSchoolsFrontDoor
      warding <- getHasRecord TheMyriadRaisedAPowerfulWarding
      colour <- getHasRecord TheMyriadTookControlOfAColourOutOfSpace
      lodge <- getHasRecord YouHaveAdvancedTheSchemesOfTheSilverTwilightLodge
      sphinx <- getHasRecord YouSolvedTheRiddleOfTheSphinx
      myriadSphinx <- getHasRecord TheMyriadSolvedTheRiddleOfTheSphinx
      steppedPast <- getHasRecord TheInvestigatorsSteppedIntoThePast
      steppedFuture <- getHasRecord TheInvestigatorsSteppedIntoTheFuture
      disappeared <- getRecordedCardCodes DisappearedUnexpectedly

      setup $ ul do
        li "gatherSets"
        li.validate violatedCausality "paradox"
        li.nested "checkCampaignLog" do
          li.validate harnessed "harnessed"
          li.validate (not harnessed) "notHarnessed"
        li "lateInvestigators"
        li "addChaosToken"
        li.validate warding "nexus"
        li.validate colour "coloursSpread"
        li.validate (not lodge) "removeLodge"
        li.validate sphinx "sphinxBonus"
        li.validate myriadSphinx "myriadSphinx"
        li.validate (notNull disappeared) "disappeared"
        li.validate steppedFuture "removeHarnessedFuture"
        li.validate steppedPast "removeGatewayToThePast"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      {- "Gather all the cards from the following encounter sets: A World Torn Down
      Again, Night of the Ritual, Agents of Aforgomon, Myriad, Nyctophobia, Thugs,
      Unleashed Chaos."

      The guide's "Agents of Aforgomon" is the data's @agents_of_chronos@.
      Unleashed Chaos goes straight to the set-aside pool: setup sets "each copy of
      Unleashed Chaos" aside and act 2c /Chaos/ or /Backfire/ is what shuffles them
      in. -}
      gather Set.AWorldTornDownAgain
      gather Set.NightOfTheRitual
      gather Set.AgentsOfChronos
      gather Set.Myriad
      gather Set.Nyctophobia
      gather Set.Thugs
      gatherAndSetAside Set.UnleashedChaos

      -- "If the investigators violated causality, also gather the Paradox
      -- encounter set."
      when violatedCausality $ gather Set.Paradox

      setAgendaDeck
        [Agendas.onceMoreUntoTheBreach, Agendas.timeRunningShort, Agendas.rageAgainstTheEnd]

      {- The past-selves deck __must be deck 2__; /Backfire/ hardcodes deck 1 as the
      present deck. Both stage-2 acts live in the one deck and 'AdvanceToAct' drops
      the sibling, because it filters the remaining stack to acts of a different
      stage. -}
      setActDeckN
        pastDeck
        [Acts.whatCameBefore, Acts.hellishHound, Acts.wardedWay, Acts.theFirstCircle]

      {- "If the investigators stepped into the future, remove Ritual Circle (A
      Harnessed Future) from the game. If the investigators stepped into the past,
      remove Ritual Circle (Gateway to the Past) from the game."

      Scenario III's resolution 3 records exactly one of the pair, so exactly one
      circle survives alongside this scenario's own /The Present, Fractured/. -}
      when steppedFuture $ removeEvery [Locations.ritualCircle_224]
      when steppedPast $ removeEvery [Locations.ritualCircle_225]
      let survivingCircles =
            [Locations.ritualCircle_224 | not steppedFuture]
              <> [Locations.ritualCircle_225 | not steppedPast]

      {- Exactly one entrance is in play, and it is the one the investigators did
      \*not* use a year ago: in this scenario you must come in the other way, or
      your past self will see you. -}
      let theEntrance = if frontDoor then Locations.rearCorridors else Locations.frontHallway
      let theOtherEntrance = if frontDoor then Locations.frontHallway else Locations.rearCorridors

      startingLocation <-
        if harnessed
          then do
            {- "If the Myriad harnessed the power of another realm: Put Featureless
            Streets into play. The starting location is Featureless Streets." -}
            setActDeckN
              presentDeck
              [ Acts.aDimensionOfItsOwn
              , Acts.theBoundary_160
              , Acts.bigAndUgly_161
              , Acts.doorwayToTheUnknown_162
              , Acts.breakingTheCircles
              ]
            setAside
              $ [ Locations.frontGates_169
                , Locations.sideBuilding_170
                , Locations.childrensPlayground_171
                , Locations.sportsField_172
                , Locations.frontHallway
                , Locations.rearCorridors
                ]
              <> sharedSetAsideLocations
              <> survivingCircles
            place Locations.featurelessStreets
          else do
            {- "Otherwise, remove Featureless Streets and act 1a from the game. The
            game begins at act 2a. Put Front Gates, Sports Field, Children's
            Playground and Side Building into play. If the investigators used the
            school's front door, put Rear Corridors into play. Otherwise, put Front
            Hallway into play. The starting location is Sports Field." -}
            removeEvery [Locations.featurelessStreets]
            setActDeckN
              presentDeck
              [ Acts.theBoundary_160
              , Acts.bigAndUgly_161
              , Acts.doorwayToTheUnknown_162
              , Acts.breakingTheCircles
              ]
            placeAll
              [ Locations.frontGates_169
              , Locations.childrensPlayground_171
              , Locations.sideBuilding_170
              , theEntrance
              ]
            setAside $ [theOtherEntrance] <> sharedSetAsideLocations <> survivingCircles
            place Locations.sportsField_172

      startAt startingLocation

      {- "Each investigator who returned to Arkham late performs setup normally, but
      begins the game at no location."

      After 'startAt', which places every investigator at the starting location --
      these seats are taken straight back out. The modifiers that keep them from
      interacting are on 'HasModifiersFor' above. -}
      selectEach theLateSeats \iid -> push $ Msg.PlaceInvestigator iid Unplaced

      -- The starting location, remembered so the end of the first round knows
      -- where to put them.
      setMeta (Just startingLocation)

      -- "Based on your difficulty level, add the following chaos token to the
      -- chaos bag for the remainder of the campaign."
      addChaosToken (campaignToken attrs.difficulty)

      {- "If the Myriad raised a powerful warding, put Nexus of Aforgomon into play
      next to the agenda deck. Otherwise, remove Nexus of Aforgomon from the game."

      Removed from the gathered deck either way: both treacheries live in this
      scenario's own encounter set and neither is ever drawn. -}
      removeEvery [Treacheries.nexusOfAforgomon, Treacheries.theColoursSpread]
      when warding $ createTreacheryAt_ Treacheries.nexusOfAforgomon NextToAgenda

      -- "If the Myriad took control of a colour out of space, put The Colour's
      -- Spread into play next to the agenda deck. Otherwise, remove it from the game."
      when colour $ createTreacheryAt_ Treacheries.theColoursSpread NextToAgenda

      -- "If you have advanced the schemes of the Silver Twilight Lodge, no changes
      -- are made. Otherwise, remove each copy of Assistance from the Lodge from
      -- the game."
      unless lodge $ removeEvery [Treacheries.assistanceFromTheLodge]

      -- "If the Myriad solved the riddle of the sphinx, place 1 doom on the current
      -- agenda." The agenda deck is set above, so agenda 1a is in play by the time
      -- this resolves.
      when myriadSphinx $ placeDoomOnAgenda 1

      {- "If [an enemy] disappeared unexpectedly, set a copy of that enemy aside,
      out of play."

      Scenario III's agenda 1b records the card code of the enemy it removed from
      the game; Scenario VI's agenda 1b spawns this copy. One copy, not every copy
      -- Sheldon's Finest and the Thugs come three to a set. -}
      for_ disappeared \code -> do
        copies <- amongGathered (CardWithCardCode code)
        setAside (take 1 copies)

      {- "Set each remaining location aside, out of play, along with Estravius
      Malone, Hound of Unmaking, Unstable Warding, Unstable Energies, The Myriad
      Gentleman (The High Priest) and each copy of Unleashed Chaos, Stirring Titan
      and Hired Thugs." -}
      setAside
        [ Enemies.estraviusMalone
        , Enemies.houndOfUnmaking
        , Enemies.theMyriadGentleman_233
        , Assets.unstableWarding
        , Events.unstableEnergies
        ]
      setAsideEvery $ cardIs Treacheries.stirringTitan
      setAsideEvery $ cardIs Enemies.hiredThugs
    -- [cultist]: "If you succeed, you get +1 skill value during skill tests until
    -- the end of the round."
    Msg.PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      roundModifier Cultist iid (AnySkillValue 1)
      pure s
    -- [tablet]: "If you fail, place 1 doom on the current agenda."
    Msg.FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _ | token.face == Tablet -> do
      placeDoomOnAgenda 1
      pure s
    -- [elder thing]: "Take 1 horror." Unconditional on both difficulty bands --
    -- hard/expert adds the auto-fail, which is on the token value above.
    Msg.ResolveChaosToken _ ElderThing iid -> do
      assignHorror iid (ChaosTokenEffectSource ElderThing) 1
      pure s
    {- An investigator who is not in play takes no turn. The investigation phase
    offers a turn to every investigator who has not ended one, with no placement
    check, so the turn is opened and immediately closed -- the
    @[SetActions … 0, ChooseEndTurn …]@ pair @TheEssexCountyExpress@ uses.
    Pushed from @BeginTurn@, so they land behind the turn-begins windows. -}
    Msg.BeginTurn iid -> do
      whenM (iid <=~> outOfPlaySeats) do
        pushAll [Msg.SetActions iid (toSource attrs) 0, Msg.ChooseEndTurn iid]
      pure s
    {- The mythos encounter draw is the one loop no modifier can reach:
    @ForInvestigator iid AllDrawEncounterCard@ guards on @isEliminated@ only, and
    the scenario runs *before* @runGameMessage@, so by the time the scenario sees
    @AllDrawEncounterCard@ the per-investigator messages do not exist yet and
    there is nothing to drop.

    The hook that does work is the @when AllDrawEncounterCard@ window the mythos
    phase checks immediately before it (@Game/Runner.hs:3175@ queues
    @[window, AllDrawEncounterCard]@ as one step): at that point the draw is still
    in the queue and can be replaced wholesale. The replacement is the engine's own
    @AllDrawEncounterCard@ handler with the out-of-play seats filtered out, Gloria
    Goldberg's draw-order choice included. -}
    Msg.CheckWindows ws | isAllDrawEncounterCardWindow ws -> do
      late <- select outOfPlaySeats
      unless (null late) do
        allMatchingDon't (== Msg.AllDrawEncounterCard)
        drawers <- filter (`notElem` late) <$> getInvestigators
        unless (null drawers) do
          active <- getActiveInvestigatorId
          selectOne (investigatorIs Investigators.gloriaGoldberg) >>= \case
            Just gloria
              | gloria `notElem` late ->
                  push
                    $ Msg.SendMessage (toTarget gloria)
                    $ Msg.ForInvestigators drawers Msg.AllDrawEncounterCard
            _ -> for_ drawers \iid -> push $ Msg.ForInvestigator iid Msg.AllDrawEncounterCard
          push $ Msg.SetActiveInvestigator active
      AWorldTornDownAgain <$> liftRunMessage msg attrs
    {- "At the end of the first round of the game, each of these investigators
    enters play at the starting location."

    @scenarioTurn@ is incremented by @BeginRound@, so it still reads 1 here. The
    starting location is read back out of scenario meta, with Sports Field as the
    fallback: act 1a can advance during the first round, and it removes Featureless
    Streets from the game after moving everyone to the Sports Field. -}
    Msg.EndRound -> do
      firstRound <- scenarioFieldMap ScenarioTurn (== 1)
      when firstRound do
        late <- select theLateSeats
        unless (null late) do
          stored <- maybe (pure Nothing) (selectOne . LocationWithId) (startingLocationOf attrs)
          fallback <- selectOne $ locationIs Locations.sportsField_172
          for_ (stored <|> fallback) \lid ->
            for_ late \iid -> push $ Msg.PlaceInvestigator iid (AtLocation lid)
      pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        -- "If no resolution was reached (each investigator was defeated): Read
        -- Resolution 2."
        NoResolution -> push R2
        -- "Each investigator earns experience equal to the Victory X value of each
        -- card in the victory display."
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        Resolution 2 -> do
          -- "In your Campaign Log, record that all times are one. Each investigator
          -- is driven insane. The investigators lose the campaign."
          record AllTimesAreOne
          resolution "resolution2"
          eachInvestigator drivenInsane
          gameOver
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> AWorldTornDownAgain <$> liftRunMessage msg attrs

-- | The starting location remembered by 'Setup'.
startingLocationOf :: ScenarioAttrs -> Maybe LocationId
startingLocationOf attrs = toResultDefault Nothing attrs.meta

{- | Is this the mythos phase's @when each investigator draws an encounter card@
window?
-}
isAllDrawEncounterCardWindow :: [Window] -> Bool
isAllDrawEncounterCardWindow = any ((== Window.AllDrawEncounterCard) . windowType)
