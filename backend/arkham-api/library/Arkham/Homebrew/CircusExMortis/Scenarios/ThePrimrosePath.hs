module Arkham.Homebrew.CircusExMortis.Scenarios.ThePrimrosePath (thePrimrosePath) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Card.CardDef
import Arkham.ChaosBag.Base (ChaosBag (..))
import Arkham.ChaosToken
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Helpers.ChaosBag (getChaosBag)
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype ThePrimrosePath = ThePrimrosePath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePrimrosePath :: Difficulty -> ThePrimrosePath
thePrimrosePath difficulty =
  scenario ThePrimrosePath ":circus-ex-mortis:017" "The Primrose Path" difficulty []

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "thePrimrosePath" a

-- Moonlit Forest variants carrying the red-cross (Victory) symbol; two of these
-- four are removed from the game at random during setup (guide p6).
victoryForests :: [CardDef]
victoryForests =
  [ Locations.moonlitForestShadowedPath
  , Locations.moonlitForestFogBank
  , Locations.moonlitForestLabyrinthOfTrees
  , Locations.moonlitForestDeadGrove
  ]

otherForests :: [CardDef]
otherForests =
  [ Locations.moonlitForestSmolderingCampfire
  , Locations.moonlitForestQuietValley
  , Locations.moonlitForestShallowRiver
  , Locations.moonlitForestGlassyLake
  , Locations.moonlitForestCircularGrove
  , Locations.moonlitForestMistyMarsh
  ]

-- Grid positions for the eight Moonlit Forest copies (guide p7 layout):
--   top row (y=1), middle sides (y=0), bottom row (y=-1). Forest Passage is the
--   center start (0,0); Remote Cabin far left (-2,0); Woodland Overlook far
--   right (2,0); Circus Encampment at the top (0,2).
forestPositions :: [Pos]
forestPositions =
  [ Pos (-1) 1
  , Pos 0 1
  , Pos 1 1
  , Pos (-1) 0
  , Pos 1 0
  , Pos (-1) (-1)
  , Pos 0 (-1)
  , Pos 1 (-1)
  ]

instance HasChaosTokenValue ThePrimrosePath where
  getChaosTokenValue iid tokenFace (ThePrimrosePath attrs) = case tokenFace of
    Skull -> do
      -- X is equal to the number of locations connected to Circus Encampment
      -- (Hard/Expert: 1 + that number).
      n <- selectCount $ connectedTo (locationIs Locations.circusEncampment)
      let extra = if isHardExpert attrs then 1 else 0
      pure $ ChaosTokenValue Skull (NegativeModifier (extra + n))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> do
      -- If a moon token was revealed during this test, it automatically fails.
      revealed <- getModifiedChaosTokenFaces =<< getSkillTestRevealedChaosTokens
      pure
        $ ChaosTokenValue ElderThing
        $ if MoonToken `elem` revealed
          then AutoFailModifier
          else if isHardExpert attrs then NegativeModifier 4 else NegativeModifier 2
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ThePrimrosePath where
  runMessage msg s@(ThePrimrosePath attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      bag <- getChaosBag
      let moonInBag = any ((== MoonToken) . (.face)) bag.chaosBagChaosTokens
          tabletInBag = any ((== Tablet) . (.face)) bag.chaosBagChaosTokens
      storyWithChooseOneM' (setTitle "title" >> p "body") do
        labeled' "useMoonlight" $ when moonInBag do
          removeChaosToken MoonToken
          addChaosToken Tablet
        labeled' "shadows" $ when tabletInBag do
          removeChaosToken Tablet
          addChaosToken MoonToken
      pure s
    Setup -> runScenarioSetup ThePrimrosePath attrs do
      gather Set.ThePrimrosePath
      gather Set.ChildrenOfTheGoat
      gather Set.IllusoryTricks
      gather Set.LunaticNight
      gather Set.SavageWoods

      setAside [Enemies.newMoonIllusionist]

      -- Randomly remove two of the four red-cross Moonlit Forests, then shuffle
      -- the remaining two together with the other six and place all eight.
      (removed, kept) <- splitAt 2 <$> shuffle victoryForests
      removeEvery removed
      forestDefs <- shuffle (kept <> otherForests)
      forestIds <- for (zip forestPositions forestDefs) \(pos, def) -> do
        lid <- placeInGrid pos def
        pure (pos, lid)

      forestPassage <- placeInGrid (Pos 0 0) Locations.forestPassage
      remoteCabin <- placeInGrid (Pos (-2) 0) Locations.remoteCabin
      woodlandOverlook <- placeInGrid (Pos 2 0) Locations.woodlandOverlook
      circusEncampment <- placeInGrid (Pos 0 2) Locations.circusEncampment
      startAt forestPassage

      -- Remote Cabin connects to the three rightmost (x=1) Moonlit Forests,
      -- Woodland Overlook to the three leftmost (x=-1), and Circus Encampment to
      -- the three top-row (y=1) copies (guide p7 + location text).
      let rightmost = [lid | (Pos x _, lid) <- forestIds, x == 1]
          leftmost = [lid | (Pos x _, lid) <- forestIds, x == -1]
          topRow = [lid | (Pos _ y, lid) <- forestIds, y == 1]
      for_ rightmost (connectBothWays remoteCabin)
      for_ leftmost (connectBothWays woodlandOverlook)
      for_ topRow (connectBothWays circusEncampment)

      setAgendaDeck [Agendas.savageNature, Agendas.bloodMoon]
      setActDeck [Acts.forestOfIllusion]
      placeDoomOnAgenda 1
    -- Moon token revealed during a skill test: seal it on the revealer's
    -- investigator card and reveal another token (campaign guide p1).
    ResolveChaosToken token MoonToken iid -> do
      sealChaosToken iid iid token
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Cultist iid -> do
      moons <- getSealedMoonTokens iid
      when (null moons) $ loseActions iid ScenarioSource 1
      pure s
    ResolveChaosToken _ Tablet iid -> do
      moons <- getSealedMoonTokens iid
      unless (null moons) $ loseActions iid ScenarioSource 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          record TheInvestigatorsWereLostInTheArkhamWoods
          resolution "resolution1"
          push R3
        Resolution 1 -> do
          record TheInvestigatorsWereLostInTheArkhamWoods
          resolution "resolution1"
          push R3
        Resolution 2 -> do
          record TheInvestigatorsBypassedTheIllusions
          resolution "resolution2"
          push R3
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> ThePrimrosePath <$> liftRunMessage msg attrs
