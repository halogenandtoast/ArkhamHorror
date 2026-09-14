module Arkham.Homebrew.DarkMatter.Scenarios.TheMachineInYellow (theMachineInYellow) where

import Arkham.Card (CardDef)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (
  addImpendingDoom,
  addMemories,
  addReminiscenceToken,
  crossOffMemories,
  doubleRevealedToken,
  doubleRevealedTokenKey,
  earnXp,
  earnXpWithBonus,
  getMemories,
  revealAnotherChaosTokenAndDouble,
  scenarioI18n,
 )
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Location.CardDefs.ThePathToCarcosa.CurtainCall qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype TheMachineInYellow = TheMachineInYellow ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The scenario is played on the Ward theatre map from /Curtain Call/, so the
grid is that scenario's own layout — the four printed locations plus the cells
the Lobby \/ Backstage doorways drop into when those locations are revealed
(@Locations\/ThePathToCarcosa\/CurtainCall\/Lobby.hs@ and @Backstage.hs@ label
them @lobbyDoorwayN@ \/ @backstageDoorwayN@).
-}
theMachineInYellow :: Difficulty -> TheMachineInYellow
theMachineInYellow difficulty =
  scenario
    TheMachineInYellow
    ":dark-matter:190"
    "The Machine in Yellow"
    difficulty
    [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
    , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
    , "lobbyDoorway2 .     .       .         backstageDoorway2"
    ]

{- | Scenario reference card, ":dark-matter:190" (the front block is
Easy \/ Standard, from @cards\/miy_01_the_machine_in_yellow.json@; the
Hard \/ Expert block is user-supplied in @act-backs.md@):

Easy \/ Standard
[skull]: -X. X is half of your "Memories" (rounded down).
[cultist]: -X. X is the number of hidden cards in your hand.
[tablet]: Reveal another token. Double that token's modifier.
[elder thing]: -4. You may choose to cross out a tally mark next to your investigator's "Memories" to automatically succeed.

Hard \/ Expert
[skull]: -X. X is your "Memories".
[cultist]: -X. X is the number of hidden cards in your hand.
[tablet]: Reveal another token. Double that token's modifier.
[elder thing]: -6. You may choose to cross out a tally mark next to your investigator's "Memories" to automatically succeed.

Only the /values/ live here; the riders are the 'ResolveChaosToken' cases in
'RunMessage' below. [tablet] prints no number on either side, which is a
modifier of 0, not "no modifier".
-}
instance HasChaosTokenValue TheMachineInYellow where
  getChaosTokenValue iid tokenFace (TheMachineInYellow attrs) = case tokenFace of
    Skull -> do
      memories <- getMemories iid
      pure $ toChaosTokenValue attrs Skull (memories `div` 2) memories
    Cultist -> do
      -- Hidden cards sit in hand as treachery \/ enemy entities, which is
      -- exactly what 'HiddenInHandCard' looks for.
      hidden <- selectCount $ InHandOf NotForPlay (InvestigatorWithId iid) <> HiddenInHandCard
      pure $ ChaosTokenValue Cultist (NegativeModifier hidden)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 0)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 6
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | "Set aside each copy of the Backstage Doorway and Lobby Doorway locations,
out of play." Each name is the unrevealed name shared by three cards; Lobby and
Backstage each pull two of them out of the set-aside pile when revealed.
-}
doorways :: [CardDef]
doorways =
  [ Locations.boxOffice
  , Locations.greenRoom
  , Locations.lightingBox
  , Locations.dressingRoom
  , Locations.rehearsalRoom
  , Locations.trapRoom
  ]

instance RunMessage TheMachineInYellow where
  runMessage msg s@(TheMachineInYellow attrs) = runQueueT $ scenarioI18n "theMachineInYellow" $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      {- "The investigators may choose to use the Maddening Delusions encounter
      set (from Return to The Path to Carcosa) instead of the Delusions
      encounter set." The gather happens inside 'Setup', which runs as one
      uninterrupted block, so the choice has to be made here; the answer is
      stored on the scenario's meta and read back below. -}
      lead <- getLead
      chooseOneM lead $ scope "maddeningDelusions" do
        labeled "useMaddeningDelusions" do
          push $ SetScenarioMeta $ toJSON True
          -- "If you do so, each investigator may add 1 tally mark next to their
          -- 'Memories'."
          eachInvestigator \iid -> chooseOneM iid do
            labeled "addMemory" $ addMemories iid 1
            labeled "doNotAddMemory" nothing
        labeled "useDelusions" nothing
      pure s
    Setup -> runScenarioSetup TheMachineInYellow attrs do
      let maddeningDelusions = toResultDefault False attrs.meta
      setup $ ul do
        li "gatherSets"
        li "maddeningDelusions"
        li.nested "placeLocations" do
          li "startAt"
        li "setAsideDoorways"
        li "setAsideYourOtherSelf"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.TheMachineInYellow
      gather Set.DarkPast
      gather Set.Rats
      if maddeningDelusions then gather Set.MaddeningDelusions else gather Set.Delusions
      {- "When gathering the Curtain Call encounter set, only gather the
      locations." Its one other gatherable card is the Royal Emissary enemy,
      which agenda 2b searches the collection for instead. -}
      gatherJustMatching Set.CurtainCall #location

      setAgendaDeck [Agendas.theThirdAct, Agendas.aNightmare, Agendas.outOfMind]
      setActDeck [Acts.awakening, Acts.theManInThePallidMask, Acts.unmasked]

      -- "Put the Theatre, Lobby, Balcony, and Backstage locations into play.
      -- Each investigator begins play at the Theatre."
      placeAll [Locations.lobby, Locations.balcony, Locations.backstage]
      startAt =<< place Locations.theatre

      setAside doorways

      -- "Set aside each copy of the Your Other Self enemy, out of play." Act 2b
      -- puts one into each investigator's threat area.
      setAsideEvery (cardIs Enemies.yourOtherSelf)
    -- [tablet]: "Reveal another token. Double that token's modifier."
    ResolveChaosToken _ Tablet iid -> do
      revealAnotherChaosTokenAndDouble iid
      pure s
    ScenarioSpecific ((== doubleRevealedTokenKey) -> True) v -> do
      doubleRevealedToken v
      pure s
    {- [elder thing]: "You may choose to cross out a tally mark next to your
    investigator's 'Memories' to automatically succeed." Only offered when there
    is a tally mark left to cross out. -}
    ResolveChaosToken _ ElderThing iid -> do
      memories <- getMemories iid
      when (memories > 0) do
        chooseOneM iid $ unscoped do
          labeled "resolveNormally" nothing
          labeled "automaticallySucceed" do
            crossOffMemories iid 1
            passSkillTest
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "If no resolution was reached (each investigator resigned or was
        defeated): Proceed to Resolution 1." -}
        NoResolution -> do
          resolution "noResolution"
          push R1
        Resolution 1 -> do
          addReminiscenceToken
          -- "Each investigator earns 2 additional experience for reliving their
          -- past memories."
          earnXpWithBonus attrs "resolution1" $ toBonus "resolution1" 2
          addImpendingDoom 2
        Resolution 2 -> do
          addReminiscenceToken
          earnXp attrs "resolution2"
        _ -> error "invalid resolution"
      when (r /= NoResolution) endOfScenario
      pure s
    _ -> TheMachineInYellow <$> liftRunMessage msg attrs
