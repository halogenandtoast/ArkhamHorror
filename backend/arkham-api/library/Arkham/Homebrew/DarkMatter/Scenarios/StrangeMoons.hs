module Arkham.Homebrew.DarkMatter.Scenarios.StrangeMoons (strangeMoons) where

import Arkham.Card (CardDef)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, isEvadeWith, isFightWith)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (
  addImpendingDoom,
  addScanningDeck,
  earnXp,
  earnXpWithBonus,
  nearestBrain,
  scenarioI18n,
 )
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Byakhee))

newtype StrangeMoons = StrangeMoons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The guide's "Suggested Location Placement" is an image; the grid comes from
the ArkhamCards flow data (@arkham-cards-zdm\/strange_moons.json@, step
@location_layout@). Every Strange Moons location uses 'symbolLabel', so the
positions are connection symbols — which also lets the six [[Simulation]]
locations, all printed with the Reality Simulator's own [circle] symbol, drop
straight into its cell when they replace it.
-}
strangeMoons :: Difficulty -> StrangeMoons
strangeMoons difficulty =
  scenario
    StrangeMoons
    ":dark-matter:153"
    "Strange Moons"
    difficulty
    [ "trefoil circle triangle"
    , "hourglass moon ."
    , ". squiggle ."
    ]

{- | Scenario reference card, ":dark-matter:153" (the front block is
Easy \/ Standard, from @cards\/moons_01_strange_moons.json@; the Hard \/ Expert
block is user-supplied in @act-backs.md@):

Easy \/ Standard
[skull]: -2. If you fail, deal 1 damage to a [[Brain]] story asset.
[cultist]: -2. If you revealed another chaos token during this test, take 1 horror.
[tablet]: Reveal 2 additional tokens for this test.
[elder thing]: -3. If this is an attack or evasion attempt against a [[Byakhee]] enemy, reveal an additional token for this test.

Hard \/ Expert
[skull]: -3. If you fail, deal 1 damage to the nearest [[Brain]] story asset.
[cultist]: -3. If you revealed another chaos token during this test, take 1 horror.
[tablet]: Reveal 2 additional tokens for this test.
[elder thing]: -4. If this is an attack or evasion attempt against a [[Byakhee]] enemy, reveal an additional token for this test.

Only the /values/ live here; the riders are the 'ResolveChaosToken' and
'FailedSkillTest' cases in 'RunMessage' below. [tablet] prints no number on
either side, which is a modifier of 0, not "no modifier".
-}
instance HasChaosTokenValue StrangeMoons where
  getChaosTokenValue iid tokenFace (StrangeMoons attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 0)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | "Put all remaining locations into play." The six [[Simulation]] locations
have scanning backs, so they are already in the scanning deck by this point;
what is left is the five Alien Chambers plus the Entrance Tunnel, which is
placed separately as the starting location.
-}
remainingLocations :: [CardDef]
remainingLocations =
  [ Locations.brainStorage
  , Locations.communicator
  , Locations.dreamDiagnostics
  , Locations.memoryScanner
  , Locations.realitySimulator
  ]

instance RunMessage StrangeMoons where
  runMessage msg s@(StrangeMoons attrs) = runQueueT $ scenarioI18n "strangeMoons" $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup StrangeMoons attrs do
      setup $ ul do
        li "gatherSets"
        li "setAsideInterstellarPredators"
        li "setAsideBrains"
        li "setAsideFeasterFromAfar"
        li "createScanningDeck"
        li.nested "placeLocations" do
          li "startAt"
        li "spawnTheGreys"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.StrangeMoons
      gather Set.DeepSpace
      -- "Set aside the Interstellar Predators encounter set, out of play."
      -- Agenda 2b shuffles the whole set into the encounter deck.
      gatherAndSetAside Set.InterstellarPredators
      setAgendaDeck
        [ Agendas.moonsOfSaturn
        , Agendas.signsFromAldebaran
        , Agendas.flightOfTheByakhees
        , Agendas.againstTheSun
        ]
      setActDeck [Acts.firstEncounter, Acts.secretsOfTheMind]

      -- "Set aside each [[Brain]] story asset, out of play." Brain Storage
      -- attaches all of them to itself when it is revealed.
      setAsideEvery (CardWithTrait Brain)

      {- "Set aside The Feaster from Afar enemy (with a scanning back)." It has to
      leave the gathered pool before the scanning deck is built — that step takes
      all the *other* encounter cards with icons on their back side. -}
      setAside [Enemies.theFeasterFromAfar]

      addScanningDeck

      -- "Put all remaining locations into play. Each investigator begins play at
      -- the Entrance Tunnel."
      placeAll remainingLocations
      entranceTunnel <- place Locations.entranceTunnel
      startAt entranceTunnel

      -- "Spawn the The Greys enemy at the Entrance Tunnel."
      createEnemyAt_ Enemies.theGreys entranceTunnel
    {- [tablet]: "Reveal 2 additional tokens for this test." -}
    ResolveChaosToken _ Tablet iid -> do
      replicateM_ 2 $ drawAnotherChaosToken iid
      pure s
    {- [cultist]: "If you revealed another chaos token during this test, take 1
    horror." Every token revealed so far this test is in the skill test's
    revealed list, so "another" is any second entry beside this [cultist]. -}
    ResolveChaosToken _ Cultist iid -> do
      revealed <- getSkillTestRevealedChaosTokens
      when (length revealed > 1) $ assignHorror iid Cultist 1
      pure s
    {- [elder thing]: "If this is an attack or evasion attempt against a
    [[Byakhee]] enemy, reveal an additional token for this test." -}
    ResolveChaosToken _ ElderThing iid -> do
      whenM (orM [isFightWith (withTrait Byakhee), isEvadeWith (withTrait Byakhee)])
        $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      {- [skull]: "If you fail, deal 1 damage to a [[Brain]] story asset" — the
      nearest one on hard/expert. Mandatory, but only if a brain is in play at
      all; they only enter play once Brain Storage is revealed. -}
      when (token.face == Skull) do
        brains <- select $ byDifficulty attrs (AssetWithTrait Brain) (nearestBrain iid)
        unless (null brains)
          $ chooseTargetM iid brains \brain -> dealAssetDamage brain Skull 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "If no resolution was reached, and at least 1 investigator resigned:
        Skip to Resolution 2.
        If no resolution was reached and each investigator was defeated: Proceed
        to Resolution 1." -}
        NoResolution -> do
          resolution "noResolution"
          anyResigned <- selectAny $ IncludeEliminated ResignedInvestigator
          push $ if anyResigned then R2 else R1
        Resolution 1 -> do
          record YouAreNotInGoodStandingWithTheMiGo
          uncoveredInhumanMethods
          {- "Add 1 tally mark under 'Impending Doom' in your Campaign Log, and 1
          additional tally mark for each [[Brain]] story asset in play." The
          agendas remove each defeated Brain from the game, so only the ones that
          survived are still in play to count. -}
          brains <- selectCount $ AssetWithTrait Brain
          addImpendingDoom (1 + brains)
          -- "Each investigator earns 2 additional experience as they gain
          -- further understanding of the agents of the King in Yellow."
          earnXpWithBonus attrs "resolution1" $ toBonus "resolution1" 2
        Resolution 2 -> do
          uncoveredInhumanMethods
          {- "If a copy of Mi-Go Scientist is in the victory display, record in
          your Campaign Log that you are not in good standing with the Mi-Go.
          Otherwise, record that you are allied with the Mi-Go." Mi-Go Scientist
          is the other side of The Greys, flipped by act 1b. -}
          scientistDefeated <-
            selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.miGoScientist
          record
            $ if scientistDefeated
              then YouAreNotInGoodStandingWithTheMiGo
              else YouAreAlliedWithTheMiGo
          addImpendingDoom 1
          earnXp attrs "resolution2"
        _ -> error "invalid resolution"
      when (r /= NoResolution) endOfScenario
      pure s
    _ -> StrangeMoons <$> liftRunMessage msg attrs

{- | Printed on both resolutions: "If there are 3 story cards in the victory
display, record in your Campaign Log that you have uncovered the cultist's
inhuman methods. Then, add 1 [elder thing] token to the chaos bag for the
remainder of the campaign for catching the eye of the King in Yellow."

The Cultist, The Miner and The Teacher are the scenario's only story cards, and
each adds itself to the victory display on its success branch only.
-}
uncoveredInhumanMethods :: ReverseQueue m => m ()
uncoveredInhumanMethods = do
  stories <- selectCount $ VictoryDisplayCardMatch $ basic #story
  when (stories >= 3) do
    record YouHaveUncoveredTheCultistsInhumanMethods
    addChaosToken ElderThing
