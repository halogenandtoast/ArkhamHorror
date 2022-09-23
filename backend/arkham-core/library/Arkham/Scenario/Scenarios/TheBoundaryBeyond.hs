module Arkham.Scenario.Scenarios.TheBoundaryBeyond
  ( TheBoundaryBeyond(..)
  , theBoundaryBeyond
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheBoundaryBeyond.Story
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait ( Trait (Ancient) )
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype TheBoundaryBeyond = TheBoundaryBeyond ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBoundaryBeyond :: Difficulty -> TheBoundaryBeyond
theBoundaryBeyond difficulty = scenario
  TheBoundaryBeyond
  "04161"
  "The Boundary Beyond"
  difficulty
  [ ".        .        .    circle  circle   .      .      ."
  , "triangle triangle star star    diamond diamond square square"
  , ".        .        .    heart   heart   .       .      ."
  ]

instance HasTokenValue TheBoundaryBeyond where
  getTokenValue iid tokenFace (TheBoundaryBeyond attrs) = case tokenFace of
    Skull -> do
      atAncientLocation <-
        selectAny $ LocationWithTrait Ancient <> locationWithInvestigator iid
      let n = if atAncientLocation then 2 else 0
      pure $ toTokenValue attrs Skull (1 + n) (2 + n)
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ toTokenValue attrs ElderThing 4 4
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheBoundaryBeyond where
  runMessage msg s@(TheBoundaryBeyond attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    Setup -> do
      iids <- getInvestigatorIds
      forgedABondWithIchtaca <- getHasRecord
        TheInvestigatorsForgedABondWithIchtaca
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      withGasoline <- headMay <$> getInvestigatorsWithSupply Gasoline
      setAsidePoisonedCount <- getSetAsidePoisonedCount

      tokens <- getTokensInBag
      let
        cultistCount = count ((== Cultist) . tokenFace) tokens
        tabletCount = count ((== Tablet) . tokenFace) tokens
        additionalSets =
          (guard (cultistCount >= 2)
            *> [EncounterSet.PnakoticBrotherhood, EncounterSet.DarkCult]
            )
            <> (guard (tabletCount >= 2)
               *> [EncounterSet.YigsVenom, EncounterSet.GuardiansOfTime]
               )
            <> (guard (cultistCount < 2 && tabletCount < 2)
               *> [ EncounterSet.PnakoticBrotherhood
                  , EncounterSet.GuardiansOfTime
                  ]
               )

      encounterDeck <-
        buildEncounterDeckExcluding [Enemies.padmaAmrita]
        $ [ EncounterSet.TheBoundaryBeyond
          , EncounterSet.TemporalFlux
          , EncounterSet.Poison
          ]
        <> additionalSets

      metropolitanCathedral <- genCard Locations.metropolitanCathedral
      zocalo <- genCard Locations.zocalo
      templeRuins <- genCard Locations.templeRuins
      xochimilco <- genCard Locations.xochimilco
      chapultepecPark <- genCard Locations.chapultepecPark
      coyoacan <- genCard Locations.coyoacan

      explorationDeck <- shuffleM =<< traverse
        genCard
        [ Locations.temploMayor_174
        , Locations.temploMayor_175
        , Locations.templesOfTenochtitlan_176
        , Locations.templesOfTenochtitlan_177
        , Locations.chapultepecHill_178
        , Locations.chapultepecHill_179
        , Locations.canalsOfTenochtitlan_180
        , Locations.canalsOfTenochtitlan_181
        , Locations.lakeXochimilco_182
        , Locations.lakeXochimilco_183
        , Locations.sacredWoods_184
        , Locations.sacredWoods_185
        , Treacheries.windowToAnotherTime
        , Treacheries.timelineDestabilization
        , Treacheries.aTearInTime
        , Treacheries.lostInTime
        ]

      setAsideCards <-
        traverse genCard
        $ [Enemies.padmaAmrita, Acts.theReturnTrip, Agendas.timeCollapsing]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      isStandalone <- getIsStandalone

      pushAll
        $ [story iids introPart1]
        <> [ story iids
               $ if forgedABondWithIchtaca then ichtacasQuest else silentJourney
           | not isStandalone
           ]
        <> [ story iids $ if foundTheMissingRelic
               then arcaneThrumming
               else growingConcern
           | not isStandalone
           ]
        <> [ story iids
               $ if rescuedAlejandro then alejandrosThoughts else anEmptySeat
           | not isStandalone
           ]
        <> [ story iids outOfGas | not isStandalone && isNothing withGasoline ]
        <> [ UseSupply iid Gasoline
           | not isStandalone
           , iid <- maybeToList withGasoline
           ]
        <> [story iids introPart2]
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , PlaceLocation metropolitanCathedral
           , PlaceLocation zocalo
           , PlaceLocation templeRuins
           , PlaceLocation xochimilco
           , PlaceLocation chapultepecPark
           , PlaceLocation coyoacan
           ]
        <> [ chooseOne
               iid
               [ targetLabel lid [MoveTo (toSource attrs) iid lid]
               | l <- [zocalo, coyoacan]
               , let lid = toLocationId l
               ]
           | iid <- iids
           ]
      TheBoundaryBeyond <$> runMessage
        msg
        (attrs
        & (decksL . at ExplorationDeck ?~ explorationDeck)
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.crossingTheThreshold, Acts.pastAndPresent]
          )
        & (agendaStackL
          . at 1
          ?~ [Agendas.theBoundaryBroken, Agendas.theBarrierIsThin]
          )
        )
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher ReplaceExplored
      pure s
    _ -> TheBoundaryBeyond <$> runMessage msg attrs
