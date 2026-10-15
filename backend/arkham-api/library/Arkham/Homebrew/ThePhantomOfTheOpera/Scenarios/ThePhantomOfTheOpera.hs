module Arkham.Homebrew.ThePhantomOfTheOpera.Scenarios.ThePhantomOfTheOpera (
  thePhantomOfTheOpera,
) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (resolveConcealed)
import Arkham.Card.CardDef (CardDef)
import Arkham.Difficulty
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.ThePhantomOfTheOpera.Helpers
import Arkham.Homebrew.ThePhantomOfTheOpera.Key
import Arkham.Homebrew.ThePhantomOfTheOpera.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement (Placement (InPlayArea, InTheShadows))
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype ThePhantomOfTheOpera = ThePhantomOfTheOpera ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The three rooms whose unrevealed sides print "Concealed mini-cards cannot
be placed". The scenario publishes it, as Sanguine Shadows and On Thin Ice do.
-}
instance HasModifiersFor ThePhantomOfTheOpera where
  getModifiersFor (ThePhantomOfTheOpera attrs) =
    modifySelect
      attrs
      ( UnrevealedLocation
          <> mapOneOf
            locationIs
            [Locations.hiddenParlor, Locations.organChamber, Locations.thePhantomsLair]
      )
      noConcealedPlacement

thePhantomOfTheOpera :: Difficulty -> ThePhantomOfTheOpera
thePhantomOfTheOpera difficulty =
  sideStory
    ThePhantomOfTheOpera
    ":the-phantom-of-the-opera:001"
    "The Phantom of the Opera"
    difficulty
    [ ". grandFoyer1 ."
    , "grandFoyer2 . grandFoyer3"
    ]

grandFoyers :: [CardDef]
grandFoyers =
  [ Locations.grandFoyer_008
  , Locations.grandFoyer_009
  , Locations.grandFoyer_010
  , Locations.grandFoyer_011
  , Locations.grandFoyer_012
  ]

parisSewers :: [CardDef]
parisSewers =
  [ Locations.parisSewers_018
  , Locations.parisSewers_019
  , Locations.parisSewers_020
  , Locations.parisSewers_021
  , Locations.parisSewers_022
  ]

phantomSetAsideCards :: [CardDef]
phantomSetAsideCards =
  [ Locations.auditorium
  , Locations.backstage
  , Locations.boxFive
  , Locations.hiddenParlor
  , Locations.organChamber
  , Locations.thePhantomsLair
  , Assets.thePersian
  , Assets.christineDaae
  , Assets.theScorpion
  , Assets.theGrasshopper
  ]
    <> parisSewers

instance HasChaosTokenValue ThePhantomOfTheOpera where
  getChaosTokenValue iid chaosTokenFace (ThePhantomOfTheOpera attrs) = case chaosTokenFace of
    Skull -> do
      n <- getCurrentActStep
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ThePhantomOfTheOpera where
  runMessage msg s@(ThePhantomOfTheOpera attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup ThePhantomOfTheOpera attrs do
      setup $ ul do
        li "gatherSets"
        li "grandFoyer"
        li "setAside"
        li "miniCards"
        li "resolvePhantom"
        unscoped $ li "shuffleRemainder"

      gather Set.ThePhantomOfTheOpera

      -- "Randomly choose two copies of Grand Foyer and remove them from the game."
      inPlay <- take 3 <$> shuffle grandFoyers
      foyers <- for (zip [1 :: Int ..] inPlay) \(n, def) -> placeLabeled ("grandFoyer" <> tshow n) def
      for_ (nonEmpty foyers) (startAt <=< sample)

      setAside phantomSetAsideCards

      {- "Find The Phantom (Lurking Mastermind) and resolve his concealed
      keyword." That is what seeds the shadows: his mini-card plus the decoys.
      Deferred to a step, because resolving the keyword reads the enemy's
      modified keywords and selects the locations to place among -- neither
      exists until the setup queue has run the messages pushed above. -}
      placeEnemy Enemies.thePhantom InTheShadows
      doStep 1 Setup

      setAgendaDeck [Agendas.sinisterForces, Agendas.faceToFace]
      setActDeck [Acts.masquerade, Acts.chasingGhosts, Acts.thePhantomsDomain, Acts.masksUponMasks]
    DoStep 1 Setup -> do
      lead <- getLead
      resolveConcealed lead =<< selectJust (enemyIs Enemies.thePhantom)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          onAgendaOne <- (== 1) <$> getCurrentAgendaStep
          push $ ScenarioResolution $ Resolution $ if onAgendaOne then 1 else 2
        Resolution 1 -> do
          record ThePhantomEscapedJustice
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        Resolution 2 -> do
          record ThePhantomDestroyedHimself
          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        Resolution 3 -> do
          record ThePhantomHasFallen
          resolutionWithXp "resolution3" $ allGainXp' attrs
          offerReward Assets.thePersian
          offerReward Assets.theGrasshopper
          endOfScenario
        Resolution 4 -> do
          record ErikLivesOnBeneathParis
          resolutionWithXp "resolution4" $ allGainXp' attrs
          offerReward Assets.thePersian
          offerReward Assets.theScorpion
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> ThePhantomOfTheOpera <$> liftRunMessage msg attrs

{- | "Any one investigator may choose to add <card> to their deck. These cards do
not count toward those investigators' deck sizes."
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

{- | The guide gives this scenario two difficulty modes; Easy plays the Standard
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
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  hardBag =
    [ Zero
    , MinusOne
    , MinusTwo
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
