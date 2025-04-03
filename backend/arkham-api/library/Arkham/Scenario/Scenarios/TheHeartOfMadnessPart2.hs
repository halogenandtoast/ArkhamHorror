module Arkham.Scenario.Scenarios.TheHeartOfMadnessPart2 (theHeartOfMadnessPart2) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardDef
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheHeartOfMadness.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheHeartOfMadnessPart2 = TheHeartOfMadnessPart2 ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfMadnessPart2 :: Difficulty -> TheHeartOfMadnessPart2
theHeartOfMadnessPart2 difficulty =
  scenarioWith
    TheHeartOfMadnessPart2
    "08648b"
    "The Heart of Madness"
    difficulty
    theHeartOfMadnessLayout
    (referenceL .~ "08648")

instance HasChaosTokenValue TheHeartOfMadnessPart2 where
  getChaosTokenValue = getChaosTokenValueFromScenario

-- Ensure the inner ring is not all mist pylons
randomizeLocations :: MonadRandom m => m [CardDef]
randomizeLocations = go =<< shuffleM (base <> pylons)
 where
  go ls =
    let innerRing = take 5 (drop 6 ls)
     in if all (`elem` innerRing) pylons
          then go =<< shuffleM ls
          else pure ls
  base =
    [ Locations.geothermalVent
    , Locations.forsakenTemple
    , Locations.libraryOfKos
    , Locations.protoplasmicPool
    , Locations.hallOfTheSunlessSea
    , Locations.subnauticalSprawl
    , Locations.subnauticalSprawl
    , Locations.vaultedCorridor
    , Locations.vaultedCorridor
    , Locations.vaultedCorridor
    ]
  pylons =
    [ Locations.mistPylon_174
    , Locations.mistPylon_175
    , Locations.mistPylon_176
    , Locations.mistPylon_177
    , Locations.mistPylon_178
    ]

instance RunMessage TheHeartOfMadnessPart2 where
  runMessage msg s@(TheHeartOfMadnessPart2 attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup TheHeartOfMadnessPart2 attrs do
      gather Set.TheHeartOfMadness
      gather Set.AgentsOfTheUnknown
      gather Set.Miasma
      gather Set.NamelessHorrors
      gather Set.Penguins
      gather Set.StirringInTheDeep
      gather Set.Tekelili
      gather Set.AncientEvils
      gather Set.ChillingCold
      gatherJust Set.StrikingFear [Treacheries.dissonantVoices, Treacheries.frozenInFear]

      setAgendaDeck [Agendas.theSealWeakens, Agendas.thatWhichHasNoName]
      setActDeck [Acts.collapseThePylons]

      place_ Locations.theGateOfYquaa

      placeGroupExact "facility" =<< randomizeLocations
      doStep 1 msg

      setAside
        $ replicate 15 Enemies.theNamelessMadness
        <> [ Acts.theFinalMirage
           , Locations.titanicRamp_182
           , Locations.titanicRamp_183
           , Locations.titanicRamp_184
           , Locations.titanicRamp_185
           ]
      addTekeliliDeck
    DoStep 1 Setup -> do
      connectAllLocations
      lead <- getLead
      ls <-
        select
          $ LocationWithUnrevealedTitle "Ancient Facility"
          <> mapOneOf LocationWithLabel ["facility7", "facility8", "facility9", "facility10", "facility11"]

      chooseTargetM lead ls (\l -> reveal l >> moveAllTo attrs l)

      pure s
    _ -> TheHeartOfMadnessPart2 <$> liftRunMessage msg attrs
