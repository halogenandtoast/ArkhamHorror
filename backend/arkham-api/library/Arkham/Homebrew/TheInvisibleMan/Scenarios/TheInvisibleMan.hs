module Arkham.Homebrew.TheInvisibleMan.Scenarios.TheInvisibleMan (theInvisibleMan) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Difficulty
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (ChaosTokenFaceModifier))
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Homebrew.TheInvisibleMan.Key
import Arkham.Homebrew.TheInvisibleMan.ScenarioDeckKeys
import Arkham.Homebrew.TheInvisibleMan.Sets qualified as Set
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement (Placement (InPlayArea))
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype TheInvisibleMan = TheInvisibleMan ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The suggested placement from the scenario card: Country Road is the hub
the investigators arrive on, with the cabin Griffin works out of at the far
corner of the map.
-}
theInvisibleMan :: Difficulty -> TheInvisibleMan
theInvisibleMan difficulty =
  sideStoryWith
    TheInvisibleMan
    ":the-invisible-man:001"
    "The Invisible Man"
    difficulty
    [ ".               sparseWoodlands sparseWoodlands moonshinersShack moonshinersShack ."
    , "meltwaterStream meltwaterStream .               .                runDownCabin     runDownCabin"
    , "lonelyFarmstead lonelyFarmstead countryRoad     countryRoad      serviceStation   serviceStation"
    ]
    -- "Put the 'c' and 'e' acts into play below the 'a' act."
    (decksLayoutL .~ ["agenda1 act1", ".       act2", ".       act3"])

{- | "Set the following cards aside, out of play: Subtle Signs, Got His Number,
Sharp Ear Out, Invisibility Serum, Griffin's Notes."
-}
invisibleManSetAsideCards :: [CardDef]
invisibleManSetAsideCards =
  [ Stories.subtleSigns
  , Stories.gotHisNumber
  , Stories.sharpEarOut
  , Assets.invisibilitySerum
  , Assets.griffinsNotes
  ]

{- | "Build the Invisible Combat and Invisible Escape Decks. This is done by
taking each story card with a matching trait that has not been set aside and
shuffling them together."
-}
invisibleCombatCards :: [CardDef]
invisibleCombatCards =
  [ Stories.everywhereAndNowhere
  , Stories.graspingAtStraws
  , Stories.shotInTheDark
  , Stories.patternRecognition
  ]

invisibleEscapeCards :: [CardDef]
invisibleEscapeCards =
  [Stories.nowhereToRun, Stories.panickedFlight, Stories.shadowedEscape]

{- | Acts 1d and 1f are removed from the act deck when they advance, so the
fight and evade values they pin down live here instead, read off the memory
each one records. Griffin's printed values are @?@, which is 'ValueX' -- zero
-- so a flat +3 is "set to 3".
-}
instance HasChaosTokenValue TheInvisibleMan where
  getChaosTokenValue iid chaosTokenFace (TheInvisibleMan attrs) = case chaosTokenFace of
    Skull -> do
      n <- field InvestigatorClues iid
      pure $ toChaosTokenValue attrs Skull (1 + n) (2 + n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheInvisibleMan where
  runMessage msg s@(TheInvisibleMan attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheInvisibleMan attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setAside"
        li "griffin"
        li "invisibleDecks"
        li "actDecks"
        unscoped $ li "shuffleRemainder"

      gather Set.TheInvisibleMan

      startAt =<< placeLabeled "countryRoad" Locations.countryRoad
      placeLabeled_ "serviceStation" Locations.serviceStation
      runDownCabin <- placeLabeled "runDownCabin" Locations.runDownCabin
      placeLabeled_ "moonshinersShack" Locations.moonshinersShack
      placeLabeled_ "sparseWoodlands" Locations.sparseWoodlands
      placeLabeled_ "meltwaterStream" Locations.meltwaterStream
      placeLabeled_ "lonelyFarmstead" Locations.lonelyFarmstead

      setAside invisibleManSetAsideCards

      -- "Put Griffin into play at Run-Down Cabin."
      createEnemyAt_ Enemies.griffin runDownCabin

      addExtraDeck InvisibleCombatDeck invisibleCombatCards
      addExtraDeck InvisibleEscapeDeck invisibleEscapeCards

      -- "Put the 'c' and 'e' acts into play below the 'a' act. During this
      -- scenario, each act deck is active at the same time."
      setAgendaDeck [Agendas.seenByTheUnseen]
      setActDeckN 1 [Acts.zeroVisibility, Acts.seeThroughToTheEnd]
      setActDeckN 2 [Acts.theQuestionOfFighting, Acts.fightingChance]
      setActDeckN 3 [Acts.theQuestionOfFleeing, Acts.runningInterference]
    -- "Griffin moves one location toward you" -- Standard waits for the test to
    -- fail, Hard moves him either way. Same split for the tablet's attack.
    ResolveChaosToken _ Cultist iid | not (isEasyStandard attrs) -> do
      moveGriffinToward iid
      pure s
    ResolveChaosToken _ Tablet iid | not (isEasyStandard attrs) -> do
      griffinAttacksIfReadyAt Tablet iid
      pure s
    FailedSkillTestWithToken iid Cultist | isEasyStandard attrs -> do
      moveGriffinToward iid
      pure s
    FailedSkillTestWithToken iid Tablet | isEasyStandard attrs -> do
      griffinAttacksIfReadyAt Tablet iid
      pure s
    ResolveChaosToken drawnToken ElderThing iid -> do
      whenAny (griffin <> at_ (locationWithInvestigator iid)) do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled "takeDamage" $ assignDamage iid ElderThing 1
          countVar 1 $ labeled "takeHorror" $ assignHorror iid ElderThing 1
          scenarioI18n $ scope "elderThing" $ countVar (worsened attrs) $ labeled "worsenToken" do
            chaosTokenEffect ElderThing drawnToken $ ChaosTokenFaceModifier [worsenedFace attrs]
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> push $ ScenarioResolution $ Resolution 1
        Resolution 1 -> do
          record TheInvisibleManEscapedJustice
          resolutionWithXp "resolution1" $ allGainXp' attrs
          offerGriffinsNotes
          endOfScenario
        Resolution 2 -> do
          record TheInvisibleManMetHisEnd
          resolutionWithXp "resolution2" $ allGainXp' attrs
          offerReward Assets.invisibilitySerum
          offerGriffinsNotes
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> TheInvisibleMan <$> liftRunMessage msg attrs

-- | "this token becomes -5 instead" ("-6 instead" on Hard/Expert).
worsened :: ScenarioAttrs -> Int
worsened attrs = if isEasyStandard attrs then 5 else 6

worsenedFace :: ScenarioAttrs -> ChaosTokenFace
worsenedFace attrs = if isEasyStandard attrs then MinusFive else MinusSix

{- | "Any one investigator may choose to add <card> to their deck. This card
does not count toward that investigator's deck size."
-}
offerReward :: (HasI18n, ReverseQueue m) => CardDef -> m ()
offerReward def = do
  lead <- getLead
  investigators <- getInvestigators
  chooseOneM lead do
    questionLabeled "addToDeck"
    questionLabeledCard def
    portraits investigators \owner -> do
      createAssetAt_ def (InPlayArea owner)
      addCampaignCardToDeck owner DoNotShuffleIn def
    labeled "declineReward" nothing

-- | Both resolutions offer the notes, but only if someone found them.
offerGriffinsNotes :: (HasI18n, ReverseQueue m) => m ()
offerGriffinsNotes =
  whenM (remembered foundGriffinsNotes) $ offerReward Assets.griffinsNotes

{- | The Invisible Man has two difficulty modes; Easy plays the Standard bag
and Expert the Hard one, which is also how its token effects pair.
-}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy -> standardBag
  Standard -> standardBag
  Hard -> hardBag
  Expert -> hardBag
 where
  standardBag =
    [ PlusOne
    , PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , Skull
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  hardBag =
    [ PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , MinusEight
    , Skull
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
