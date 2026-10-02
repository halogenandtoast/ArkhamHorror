module Arkham.Homebrew.CircusExMortis.Scenarios.RedSunrise (redSunrise) where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getLocationOf, withLocationOf)
import Arkham.Helpers.Xp (XpBonus (NoBonus))
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Id (InvestigatorId)
import Arkham.Layout (GridTemplateRow)
import Arkham.Location.Grid (Pos (..))
import Arkham.Location.Group
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move (moveToward)
import Arkham.Message.Story (StoryMessage (PlaceStory))
import Arkham.Placement (Placement (AsSelfLocation))
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Monster))
import Arkham.Trait qualified as Trait

newtype RedSunrise = RedSunrise ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The map is a true grid rather than a layout string, because almost every card
in the scenario asks about "your row" and 'LocationInRowOf' reads grid positions.
Above is nearer Ritual Clearing (higher y), below nearer Forgotten Trail.
-}
redSunrise :: Difficulty -> RedSunrise
redSunrise difficulty =
  scenario
    RedSunrise
    ":circus-ex-mortis:153"
    "Red Sunrise"
    difficulty
    redSunriseLayout

-- | One copy of each same-named set is removed from the game at random (guide p28).
foothillSlopes :: [CardDef]
foothillSlopes =
  [ Locations.foothillSlope_162
  , Locations.foothillSlope_163
  , Locations.foothillSlope_164
  , Locations.foothillSlope_165
  ]

mountainStreams :: [CardDef]
mountainStreams =
  [ Locations.mountainStream_166
  , Locations.mountainStream_167
  , Locations.mountainStream_168
  , Locations.mountainStream_169
  ]

openForests :: [CardDef]
openForests =
  [Locations.openForest_170, Locations.openForest_171, Locations.openForest_172]

shadowedWildernesses :: [CardDef]
shadowedWildernesses =
  [ Locations.shadowedWilderness_173
  , Locations.shadowedWilderness_174
  , Locations.shadowedWilderness_175
  , Locations.shadowedWilderness_176
  , Locations.shadowedWilderness_177
  ]

{- | Eight physical copies, two per column. The @b@ copies exist only so that two
copies of one column can be beside two different rows at once: 'Arkham.Id.StoryId' is
the card code, so sharing a code would silently drop one of them — and with it a row's
only route upward.
-}
pathsForward :: [CardDef]
pathsForward =
  [ Stories.pathForward_178
  , Stories.pathForward_178a
  , Stories.pathForward_179
  , Stories.pathForward_179a
  , Stories.pathForward_180
  , Stories.pathForward_180a
  , Stories.pathForward_181
  , Stories.pathForward_181a
  ]

-- | Centre a row of @n@ locations on column 0 at height @y@.
rowPositions :: Int -> Int -> [Pos]
rowPositions y n =
  let left = (n - 1) `div` 2
   in [Pos x y | x <- [negate left .. n - 1 - left]]

{- | Locations are placed on the grid (every "your row" card reads 'LocationInRowOf',
which is grid positions), but the grid's generated layout stacks the rows flush left. So
setup replaces it with this: one label per row group, each spanning the columns it needs,
which is what centres them. Each group's box is that named area and lays its members out
inside it.
-}
redSunriseLayout :: [GridTemplateRow]
redSunriseLayout =
  [ ". . . ritualClearing ritualClearing . . . . ."
  , "row4 row4 row4 row4 row4 row4 row4 row4 path4 path4"
  , ". row3 row3 row3 row3 row3 row3 . path3 path3"
  , ". row2 row2 row2 row2 row2 row2 . path2 path2"
  , ". . row1 row1 row1 row1 . . path1 path1"
  , ". . . forgottenTrail forgottenTrail . . . . ."
  ]

-- | One group per multi-location row, keyed by its grid row.
rowGroupKey :: Int -> LocationGroupKey
rowGroupKey y = LocationGroupKey $ "row" <> tshow y

actOneFor :: HasGame m => m CardDef
actOneFor = do
  struckDown <- getHasRecord TheInvestigatorsStruckDownBlake
  unmasked <- getHasRecord TheInvestigatorsUnmaskedBlake
  pure
    $ if struckDown
      then Acts.forestOfGiantsVI
      else if unmasked then Acts.forestOfGiantsVII else Acts.forestOfGiantsVIII

instance HasChaosTokenValue RedSunrise where
  getChaosTokenValue iid tokenFace (RedSunrise attrs) = case tokenFace of
    Skull -> do
      beastInRow <- runDefaultMaybeT False do
        lid <- MaybeT $ getLocationOf iid
        lift $ selectAny $ EnemyAt (rowOf lid) <> mapOneOf EnemyWithTrait [Monster, Trait.Cultist]
      pure
        $ if beastInRow
          then toChaosTokenValue attrs Skull 3 4
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ toChaosTokenValue attrs Cultist 0 1
    Tablet -> pure $ toChaosTokenValue attrs Tablet 0 1
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    MoonToken -> pure moonTokenValue
    otherFace -> getChaosTokenValue iid otherFace attrs

-- | "X is the number of locations in your row."
getRowSizeOf :: HasGame m => InvestigatorId -> m Int
getRowSizeOf iid = getLocationOf iid >>= maybe (pure 0) getRowSize

{- | The elder thing token's "move the nearest enemy once toward your location". It
names no trait restriction, so unlike Shadowed Wilderness it can move an Elite, and
"the nearest" is singular — so a tie is the investigator's choice.
-}
moveNearestEnemyTowardInvestigator :: ReverseQueue m => InvestigatorId -> m ()
moveNearestEnemyTowardInvestigator iid = withLocationOf iid \lid -> do
  enemies <- nearestEnemiesAbleToMoveToward lid AnyEnemy
  chooseTargetM iid enemies \eid -> moveToward eid (LocationWithId lid)

instance RunMessage RedSunrise where
  runMessage msg s@(RedSunrise attrs) = runQueueT $ scenarioI18n "redSunrise" $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM (setTitle "title" >> p "body1" >> p "body2") do
        labeled "timeIsOfTheEssence" $ addChaosToken Cultist
        labeled "easyDoesIt" $ addChaosToken Tablet
      pure s
    Setup -> runScenarioSetup RedSunrise attrs do
      discovered <- getHasRecord TheInvestigatorsDiscoveredTheRitualsLocation
      actOne <- actOneFor
      citizens <- getRecordCount GroupsOfCitizensWereSavedFromTheCircus
      let agendaOne = if discovered then Agendas.fadingSunlightVI else Agendas.fadingSunlightVII

      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li "pathForward"
          li.nested "checkAgenda" do
            li.validate discovered "discoveredRitual"
            li.validate (not discovered) "followTheCult"
          li.nested "checkAct" do
            li.validate (actOne == Acts.forestOfGiantsVI) "struckDownBlake"
            li.validate (actOne == Acts.forestOfGiantsVII) "unmaskedBlake"
            li.validate (actOne == Acts.forestOfGiantsVIII) "clashedWithBlake"
          li "doom"
          li "setAside"
          unscoped $ li "shuffleRemainder"

      additionalRules "rows"

      gather Set.RedSunrise
      gather Set.ChildrenOfTheGoat
      gather Set.IllusoryTricks
      gather Set.NewMoonDaredevils
      gather Set.PrimordialEvils
      gather Set.SavageWoods

      -- "For each set of locations with matching names, choose one copy at random
      -- and remove it from the game."
      keptRows <- for [openForests, foothillSlopes, mountainStreams, shadowedWildernesses] \defs -> do
        shuffled <- shuffle defs
        case shuffled of
          [] -> pure []
          (removed : kept) -> do
            removeEvery [removed]
            pure kept

      {- Each multi-location row is drawn as one box: every member of a row connects
      to every member of the row below it, so a flat map would draw 12 lines where the
      box draws one. Rows keep their grid positions too, because 'LocationInRowOf' (and
      so every "your row" card) reads them. -}
      setLocationGroups [LocationGroup (rowGroupKey y) GroupRow | y <- [1 .. 4]]

      {- placeInGrid labels a location by its position (pos0000), but the layout names
      the two ungrouped locations, so relabel them to match. Grouped locations need no
      label: their box holds the grid area and lays them out inside it. -}
      forgottenTrail <- placeInGrid (Pos 0 0) Locations.forgottenTrail
      setLocationLabel forgottenTrail "forgottenTrail"
      startAt forgottenTrail
      for_ (zip [1 ..] keptRows) \(y, defs) -> do
        lids <- for (zip (rowPositions y (length defs)) defs) (uncurry placeInGrid)
        for_ (zip [0 ..] lids) \(i, lid) ->
          push $ SetLocationGroup lid (GroupMembership (rowGroupKey y) i)
      ritualClearing <- placeInGrid (Pos 0 5) Locations.ritualClearing
      setLocationLabel ritualClearing "ritualClearing"
      -- Overrides the layout the grid generates from positions.
      push $ SetLayout redSunriseLayout

      {- "Choose four of the Path Forward story cards at random and remove them from
      the game. Shuffle each of the remaining copies together and place one of them
      facedown beside each row of 2 or more locations." Eight copies, four removed,
      four rows of 2+ — one per row. -}
      pathCards <- shuffle =<< traverse genCard pathsForward
      let (placedPaths, removedPaths) = splitAt 4 pathCards
      removeCards removedPaths
      {- One per row of 2+ locations, facedown (the card's own constructor sets
      'flippedL'). 'AsSelfLocation' gives the copy a grid cell of its own beside the
      row, which is "in play, but not at any location". -}
      for_ (zip [1 :: Int ..] placedPaths) \(y, card) ->
        push $ StoryMessage $ PlaceStory card (AsSelfLocation $ "path" <> tshow y)

      setAgendaDeck [agendaOne]
      setActDeck [actOne, Acts.impendingZenith]

      -- "Place 4 doom on agenda 1a. Reduce this amount by X, where X groups of
      -- citizens were saved from the circus."
      let doom = max 0 (4 - citizens)
      placeDoomOnAgenda doom

      setAside
        [ Enemies.theCultEnMasseLeaderlessFanaticism
        , Enemies.theCultEnMasseBlackGoatsRapture
        , Enemies.theCultEnMasseRingmastersFervor
        , Enemies.devoteeOfTheThousand
        ]
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      moveNearestEnemyTowardInvestigator iid
      pure s
    -- "If you do not succeed by X": a pass whose margin is under X counts too.
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      x <- getRowSizeOf iid
      when (n < x) $ rowShortfall attrs token.face iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      rowShortfall attrs token.face iid
      when (token.face == ElderThing && isEasyStandard attrs)
        $ moveNearestEnemyTowardInvestigator iid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R1
        Resolution 1 -> do
          resolution "resolution1"
          record TheInvestigatorsDidNotArriveInTime
          record ShubNiggurathReignsOverAnEclipsedWorld
          selectEach UneliminatedInvestigator $ push . InvestigatorKilled (toSource attrs)
          gameOver
          endOfScenario
        Resolution 2 -> do
          cultDefeated <- selectAny $ VictoryDisplayCardMatch $ basic $ CardWithTitle "The Cult En Masse"
          unless cultDefeated $ record TheCultRallies
          addChaosToken ElderThing
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs NoBonus
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> RedSunrise <$> liftRunMessage msg attrs

{- | "If you do not succeed by X (where X is the number of locations in your row)":
Cultist draws an encounter card, Tablet costs an action.
-}
rowShortfall
  :: (ReverseQueue m, Sourceable source) => source -> ChaosTokenFace -> InvestigatorId -> m ()
rowShortfall source face iid = case face of
  Cultist -> drawEncounterCard iid source
  Tablet -> loseActions iid source 1
  _ -> pure ()
