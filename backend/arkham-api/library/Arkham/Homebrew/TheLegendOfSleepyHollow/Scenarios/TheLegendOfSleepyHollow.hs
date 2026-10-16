module Arkham.Homebrew.TheLegendOfSleepyHollow.Scenarios.TheLegendOfSleepyHollow (
  theLegendOfSleepyHollow,
) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Difficulty
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Helpers.SkillTest (isEvadeWith, isFightWith)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Homebrew.TheLegendOfSleepyHollow.Key
import Arkham.Homebrew.TheLegendOfSleepyHollow.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (ModifierType (ChaosTokenFaceModifier))
import Arkham.Placement (Placement (InPlayArea, OutOfPlay))
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Geist))
import Arkham.Zone

newtype TheLegendOfSleepyHollow = TheLegendOfSleepyHollow ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The suggested placement from the scenario card: Wiley's Swamp is the hub,
with the farmstead and the tree trailing off one way and the church, the
schoolhouse and White Plains the other.
-}
theLegendOfSleepyHollow :: Difficulty -> TheLegendOfSleepyHollow
theLegendOfSleepyHollow difficulty =
  sideStory
    TheLegendOfSleepyHollow
    ":the-legend-of-sleepy-hollow:001"
    "The Legend of Sleepy Hollow"
    difficulty
    [ ".              whitePlains      derelictSchoolhouse"
    , "oldDutchChurch wileysSwamp      ."
    , ".              majorAndresTree  vanTasselFarmstead"
    ]

-- | "all copies of Disembodied Skull, Secretive Hag, and Van Brunt's Reward"
sleepyHollowSetAsideCards :: [CardDef]
sleepyHollowSetAsideCards =
  [Assets.ichabodCrane, Assets.ichabodsBooks]
    <> replicate 2 Enemies.disembodiedSkull
    <> replicate 2 Enemies.secretiveHag
    <> replicate 4 Assets.vanBruntsReward

instance HasChaosTokenValue TheLegendOfSleepyHollow where
  getChaosTokenValue iid chaosTokenFace (TheLegendOfSleepyHollow attrs) = case chaosTokenFace of
    Skull -> do
      n <- selectCount LocationWithAnyDoom
      pure $ toChaosTokenValue attrs Skull (1 + n) (2 + n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheLegendOfSleepyHollow where
  runMessage msg s@(TheLegendOfSleepyHollow attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheLegendOfSleepyHollow attrs do
      setup $ ul do
        li "gatherSets"
        li "headlessHorseman"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        li "placeLocations"

      gather Set.TheLegendOfSleepyHollow

      -- "Put The Headless Horseman enemy next to the agenda deck, out of play."
      placeEnemy Enemies.theHeadlessHorseman (OutOfPlay SetAsideZone)

      setAside sleepyHollowSetAsideCards

      startAt =<< placeLabeled "vanTasselFarmstead" Locations.vanTasselFarmstead
      placeLabeled_ "majorAndresTree" Locations.majorAndresTree
      placeLabeled_ "wileysSwamp" Locations.wileysSwamp
      placeLabeled_ "derelictSchoolhouse" Locations.derelictSchoolhouse
      placeLabeled_ "oldDutchChurch" Locations.oldDutchChurch
      placeLabeled_ "whitePlains" Locations.whitePlains

      setAgendaDeck [Agendas.onTheWingsOfTheWind]
      setActDeck
        [Acts.theTaleOfIchabodCrane, Acts.theHessianHorseman, Acts.banishingTheSpectre]
    ResolveChaosToken drawnToken Cultist iid -> do
      -- Real doom: Disembodied Skull's stand-in doom is for enemy and treachery
      -- effects, and the scenario reference is neither.
      whenAny (locationWithInvestigator iid <> LocationWithAnyDoom) do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled "takeDamage" $ assignDamage iid Cultist 1
          scenarioI18n $ scope "cultist" $ countVar (worsened attrs) $ labeled "worsenToken" do
            chaosTokenEffect Cultist drawnToken $ ChaosTokenFaceModifier [worsenedFace attrs]
      pure s
    ResolveChaosToken drawnToken Tablet iid -> do
      whenAny (EnemyWithTrait Geist <> at_ (locationWithInvestigator iid)) do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeled "takeHorror" $ assignHorror iid Tablet 1
          scenarioI18n $ scope "tablet" $ countVar (worsened attrs) $ labeled "worsenToken" do
            chaosTokenEffect Tablet drawnToken $ ChaosTokenFaceModifier [worsenedFace attrs]
      pure s
    ResolveChaosToken _ ElderThing _ -> do
      -- "If this is a fight or evade test against The Headless Horseman, it
      -- automatically fails instead."
      confronting <- orM [isFightWith headlessHorseman, isEvadeWith headlessHorseman]
      when confronting failSkillTest
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> push $ ScenarioResolution $ Resolution 1
        Resolution 1 -> do
          record TheHeadlessHorsemanContinuesToHauntSleepyHollow
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsBanishedTheHeadlessHorseman
          resolutionWithXp "resolution2" $ allGainXp' attrs
          eachInvestigator \iid -> do
            chooseOneM iid do
              questionLabeledCard Assets.vanBruntsReward
              labeled "addToDeck" do
                createAssetAt_ Assets.vanBruntsReward (InPlayArea iid)
                addCampaignCardToDeck iid DoNotShuffleIn Assets.vanBruntsReward
              labeled "declineReward" nothing
          offerIchabod
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> TheLegendOfSleepyHollow <$> liftRunMessage msg attrs

-- | "make this token -4 instead" ("-5 instead" on Hard/Expert).
worsened :: ScenarioAttrs -> Int
worsened attrs = if isEasyStandard attrs then 4 else 5

worsenedFace :: ScenarioAttrs -> ChaosTokenFace
worsenedFace attrs = if isEasyStandard attrs then MinusFour else MinusFive

{- | "Any one investigator may choose to add Ichabod Crane to his or her deck.
These cards do not count toward each investigator's deck size."
-}
offerIchabod :: (HasI18n, ReverseQueue m) => m ()
offerIchabod = do
  lead <- getLead
  investigators <- getInvestigators
  chooseOneM lead do
    questionLabeled "addToDeck"
    questionLabeledCard Assets.ichabodCrane
    portraits investigators \owner -> do
      createAssetAt_ Assets.ichabodCrane (InPlayArea owner)
      addCampaignCardToDeck owner DoNotShuffleIn Assets.ichabodCrane
    labeled "declineReward" nothing

{- | Legend of Sleepy Hollow has two difficulty modes; Easy plays the Standard
bag and Expert the Hard one, which is also how its token effects pair.
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
