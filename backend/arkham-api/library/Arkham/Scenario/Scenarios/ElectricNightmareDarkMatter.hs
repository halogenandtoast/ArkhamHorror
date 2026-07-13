module Arkham.Scenario.Scenarios.ElectricNightmareDarkMatter (electricNightmareDarkMatter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.DarkMatter.Helpers
import Arkham.Campaigns.DarkMatter.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

newtype ElectricNightmareDarkMatter = ElectricNightmareDarkMatter ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

electricNightmareDarkMatter :: Difficulty -> ElectricNightmareDarkMatter
electricNightmareDarkMatter difficulty =
  scenario ElectricNightmareDarkMatter "z-dark-matter-053" "Electric Nightmare" difficulty []

-- | Local scenario i18n scope: darkMatter.electricNightmare.*
scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "electricNightmare" a

instance HasChaosTokenValue ElectricNightmareDarkMatter where
  getChaosTokenValue iid tokenFace (ElectricNightmareDarkMatter attrs) = case tokenFace of
    Skull -> do
      -- Easy/Standard: -X where X is half of your Memories (rounded down).
      -- Hard/Expert: -X where X is your Memories.
      memories <- getMemories iid
      pure $ toChaosTokenValue attrs Skull (memories `div` 2) memories
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ElectricNightmareDarkMatter where
  runMessage msg s@(ElectricNightmareDarkMatter attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      -- guide p6: each investigator with 3 or fewer Memories reads
      -- Desynchronization and adds the Desync weakness to their deck.
      checkDesynchronization
      pure s
    Setup -> runScenarioSetup ElectricNightmareDarkMatter attrs do
      setUsesGrid

      gather Set.DarkMatterElectricNightmare
      gather Set.DarkMatterEndtimes
      gather Set.DarkMatterDarkPast
      gatherAndSetAside Set.DarkMatterTheBoogeyman

      -- Randomly select one version of act 1 (the other two are removed from
      -- the game, i.e. never added to the act deck).
      version <-
        sample
          $ Acts.publicSchool187V10DarkMatter
          :| [Acts.publicSchool187V20DarkMatter, Acts.publicSchool187V30DarkMatter]
      setActDeck [version, Acts.psychoanalysisDarkMatter, Acts.facingYourFearsDarkMatter]
      setAgendaDeck [Agendas.figmentOfYourImaginationDarkMatter, Agendas.itDarkMatter]

      -- Undefined Room locations + Entrance Hall (A Shimmer in the Wall) are set
      -- aside; they enter play via the act / School Grounds later.
      setAside
        [ Locations.cafeteriaDarkMatter
        , Locations.classroomK2DarkMatter
        , Locations.gymnasiumDarkMatter
        , Locations.biologyLabDarkMatter
        , Locations.libraryDarkMatter
        , Locations.entranceHallDarkMatter
        ]

      schoolGrounds <- placeInGrid (Pos 0 0) Locations.schoolGroundsDarkMatter
      startAt schoolGrounds
      placeAsset_ Assets.majaDarkMatter (AttachedToLocation schoolGrounds)

      -- Avatars (Reintegrated) and the K2-PS187 functionality assets set aside.
      setAside
        [ Stories.reintegratedDarkMatter_062
        , Stories.reintegratedDarkMatter_063
        , Stories.reintegratedDarkMatter_064
        , Stories.reintegratedDarkMatter_065
        , Assets.k2PS18725FunctionalityDarkMatter
        , Assets.k2PS18750FunctionalityDarkMatter
        , Assets.k2PS18775FunctionalityDarkMatter
        , Assets.k2PS187100FunctionalityDarkMatter
        ]

      -- If an investigator has been infected by the cybervirus, they begin the
      -- scenario with Cybervirus in their hand.
      infected <- getRecordSet HasBeenInfectedByTheCybervirus
      iids <- allInvestigators
      for_ iids \iid ->
        when (recorded (unInvestigatorId iid) `elem` infected) do
          card <- genCard Enemies.cybervirusDarkMatter
          addToHand iid (only card)
    ResolveChaosToken _ Tablet iid -> do
      -- Reveal another token. ("Double that token's modifier" is a HARD/EXPERT
      -- clause; doubling the freshly-drawn token requires a per-draw token
      -- modifier hook the engine does not cleanly expose here.)
      -- TODO(homebrew): double the modifier of the newly drawn token.
      push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> do
          -- If you fail and you have a hidden card in your hand, take 1 horror.
          hasHidden <- selectAny (InvestigatorWithId iid <> InvestigatorWithHiddenCard)
          when hasHidden $ assignHorror iid attrs 1
        _ -> pure ()
      pure s
    ScenarioSpecific "switchLocations" v -> do
      -- Central switch handler (guide p7 "Switching Locations"). Two locations
      -- trade grid positions; all contents remain on the same location. During
      -- act 1 (Public School 187), "Locations cannot be switched with each
      -- other," so we suppress the switch entirely.
      let (a, b) = toResult v :: (LocationId, LocationId)
      inAct1 <- selectAny (ActWithStep 1)
      unless inAct1 do
        grid <- getGrid
        case (findInGrid a grid, findInGrid b grid) of
          (Just posA, Just posB) | posA /= posB -> do
            pushAll [PlaceGrid (GridLocation posB a), PlaceGrid (GridLocation posA b)]
            checkAfter $ Window.ScenarioEvent "switched" Nothing (toJSON (a, b))
          _ -> pure ()
      pure s
    ScenarioResolution res -> scope "resolutions" do
      reintegratedCount <-
        selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTitle "Reintegrated"
      reminiscenceInVictory <-
        selectAny
          $ VictoryDisplayCardMatch
          $ basic
          $ mapOneOf
            cardIs
            [ Treacheries.reminiscencePledgeDarkMatter
            , Treacheries.reminiscenceSecretsDarkMatter
            , Treacheries.reminiscenceCovenantDarkMatter
            ]
      let addReminiscenceToken = when reminiscenceInVictory $ addChaosToken ElderThing

      -- NoResolution routes to the loss (R1) with no Reintegrated cards, or the
      -- partial ending (R2) if at least one child was reintegrated.
      let resolved = case res of
            NoResolution -> if reintegratedCount >= 1 then 2 else 1
            Resolution n -> n

      case resolved of
        1 -> do
          record YouAreTrappedInAVirtualNightmare
          eachInvestigator drivenInsane
          resolution "resolution1"
          gameOver
        2 -> do
          record YouPartiallyRestoredTheSanityOfK2PS187
          offerK2Reward $ case reintegratedCount of
            1 -> Just Assets.k2PS18725FunctionalityDarkMatter
            2 -> Just Assets.k2PS18750FunctionalityDarkMatter
            3 -> Just Assets.k2PS18775FunctionalityDarkMatter
            _ -> Nothing
          addReminiscenceToken
          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        3 -> do
          record YouFullyRestoredTheSanityOfK2PS187
          offerK2Reward $ Just Assets.k2PS187100FunctionalityDarkMatter
          addReminiscenceToken
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        _ -> error "Invalid resolution"
      pure s
    _ -> ElectricNightmareDarkMatter <$> liftRunMessage msg attrs

{- | "An investigator may choose to add the K2-PS187 (X%) permanent story asset
to their deck." Optional; a single investigator may take it.
-}
offerK2Reward :: ReverseQueue m => Maybe CardDef -> m ()
offerK2Reward Nothing = pure ()
offerK2Reward (Just def) = do
  investigators <- allInvestigators
  lead <- getLead
  chooseOneM lead do
    labeled "No investigator adds it to their deck" nothing
    targets investigators \iid -> addCampaignCardToDeck iid DoNotShuffleIn def
