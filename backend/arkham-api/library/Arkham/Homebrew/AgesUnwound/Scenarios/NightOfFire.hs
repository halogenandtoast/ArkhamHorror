{- | Scenario I. A street chase through Arkham the night the investigators' home
burns.

The eight Arkham locations are not a map: Rivertown starts in play and the other
seven sit in a face-down __Arkham Streets deck__ that the acts, the agendas, two
treacheries and the [cultist] token deal from one card at a time. That one
instruction lives in
"Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers"; every card that
prints it calls the helper instead of re-deriving it. There is therefore no grid
layout to print.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire (nightOfFire) where

import Arkham.Helpers.FlavorText (flavor, h, li, p, setup, ul)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype NightOfFire = NightOfFire ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The Arkham Streets map, laid out so a street can be dealt into its cell in any
order and still draw cleanly.

The connection graph is a wheel: Independence Square (@square@) touches every
location except A Place to Hide, the other six form the cycle
circle -> moon -> triangle -> t -> hourglass -> diamond -> circle, and
A Place to Hide (@plus@) hangs off Twisting Alleys (@diamond@).

Placing the hub in the centre, the six-cycle around it and the pendant beside its
only neighbour puts all 13 connections between grid-adjacent cells with no two
connection lines crossing. Each location carries 'symbolLabel', so the cell is
its symbol and a freshly dealt street lands in the right place by itself.
-}
nightOfFire :: Difficulty -> NightOfFire
nightOfFire difficulty =
  scenario
    NightOfFire
    ":ages-unwound:001"
    "Night of Fire"
    difficulty
    [ ".      moon     triangle"
    , "circle square   t"
    , "plus   diamond  hourglass"
    ]

{- | Scenario reference card, @:ages-unwound:001@:

Easy / Standard
[skull]: -X. X is half the number of locations in the Arkham Streets deck (rounded down).
[cultist]: Reveal another token. If you fail, after this test ends, move to a new Arkham Streets location.
[tablet]: -3 (-1 instead if you have moved this turn).

Hard / Expert
[skull]: -X. X is the number of locations in the Arkham Streets deck.
[cultist]: Reveal another token. If you fail, after this test ends, move to a new Arkham Streets location.
[tablet]: -4 (-2 instead if you have moved this turn).

[skull] rewards running the deck down; [tablet] rewards having run this turn.
There is deliberately no [elder thing] effect: the chaos bag has no such token
until Scenario III's resolutions add one.
-}
instance HasChaosTokenValue NightOfFire where
  getChaosTokenValue iid tokenFace (NightOfFire attrs) = case tokenFace of
    Skull -> do
      n <- length <$> getArkhamStreetsDeck
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (n `div` 2) n)
    -- The reveal-another and the move rider are the 'ResolveChaosToken' and
    -- 'FailedSkillTest' cases below; the token itself has no value.
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> do
      moved <- iid <=~> InvestigatorThatMovedDuringTurn
      pure
        $ if moved
          then toChaosTokenValue attrs Tablet 1 2
          else toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage NightOfFire where
  runMessage msg s@(NightOfFire attrs) = runQueueT $ nightOfFireI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup NightOfFire attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeRivertown" do
          li "startAt"
        li "arkhamStreetsDeck"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      {- "Gather all cards from the following encounter sets: Night of Fire,
      Agents of Aforgomon, Nyctophobia, Thugs and Unravelling Ages."

      The 2022 guide's "Agents of Aforgomon" and "Unravelling Ages" are the card
      data's @agents_of_chronos@ and @unravelling_years@; the data wins on names.
      Agents of Chronos is gathered straight to the set-aside pool rather than
      the encounter deck -- see the set-aside step below. -}
      gather Set.NightOfFire
      gather Set.Nyctophobia
      gather Set.Thugs
      gather Set.UnravellingYears
      gatherAndSetAside Set.AgentsOfChronos

      {- "Set Myriad Assassin, Irregulars, each copy of Eternity's Sentinel, each
      copy of Time Spirit and the Agents of Aforgomon encounter set aside, out of
      play." Agenda 1b shuffles the first two plus the whole set back in. -}
      setAside
        [ Enemies.myriadAssassin
        , Enemies.irregulars
        , Enemies.eternitysSentinel_016
        , Enemies.eternitysSentinel_017
        ]
      setAsideEvery $ cardIs Enemies.timeSpirit

      setAgendaDeck [Agendas.hunted, Agendas.watched, Agendas.gazeOfThreeEyes]
      setActDeck [Acts.onTheLam, Acts.gettingYourBearings, Acts.slippingTheNet]

      {- "Put the Rivertown location into play. (It is on the revealed side of
      one of the Arkham Streets locations.) Each investigator begins play at
      Rivertown." -}
      startAt =<< place Locations.rivertown

      {- "Shuffle the remaining locations into a separate deck, Arkham Streets
      side faceup." Placing a location card always puts it into play on its
      unrevealed side, so the deck is simply the other seven location cards. -}
      addExtraDeck ArkhamStreetsDeck
        =<< shuffleM
          [ Locations.independenceSquare
          , Locations.easttown
          , Locations.frenchHill
          , Locations.twistingAlleys
          , Locations.winchmoreHouse
          , Locations.crampedPassage
          , Locations.aPlaceToHide
          ]
    -- [cultist]: "Reveal another token."
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    -- [cultist]: "If you fail, after this test ends, move to a new Arkham
    -- Streets location."
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      withSkillTest \sid ->
        afterThisTestResolves sid $ moveToNewArkhamStreetsLocation Cultist iid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "If no resolution was reached (each investigator was defeated): Read
        Resolution 1." -}
        NoResolution -> push R1
        -- "In your Campaign Log, record that your hunters found a new quarry."
        Resolution 1 -> do
          record YourHuntersFoundANewQuarry
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        {- "record that the investigators survived the night of fire" + "Each
        investigator earns 2 bonus experience from their desperate flight." -}
        Resolution 2 -> do
          record TheInvestigatorsSurvivedTheNightOfFire
          resolutionWithXp "resolution2"
            $ allGainXpWithBonus' attrs (toBonus "desperateFlight" 2)
          endOfScenario
        -- "record that the investigators slew their strange observer"
        Resolution 3 -> do
          record TheInvestigatorsSlewTheirStrangeObserver
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        -- "record that the investigators escaped the night of fire" + 2 bonus
        Resolution 4 -> do
          record TheInvestigatorsEscapedTheNightOfFire
          resolutionWithXp "resolution4"
            $ allGainXpWithBonus' attrs (toBonus "desperateFlight" 2)
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> NightOfFire <$> liftRunMessage msg attrs
