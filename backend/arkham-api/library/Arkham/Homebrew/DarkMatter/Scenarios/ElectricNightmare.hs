module Arkham.Homebrew.DarkMatter.Scenarios.ElectricNightmare (electricNightmare) where

import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.Helpers
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Card
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Id
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Window qualified as Window

newtype ElectricNightmare = ElectricNightmare ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

electricNightmare :: Difficulty -> ElectricNightmare
electricNightmare difficulty =
  scenario ElectricNightmare ":dark-matter:053" "Electric Nightmare" difficulty []

-- | Local scenario i18n scope: darkMatter.electricNightmare.*
scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "electricNightmare" a

instance HasChaosTokenValue ElectricNightmare where
  getChaosTokenValue iid tokenFace (ElectricNightmare attrs) = case tokenFace of
    Skull -> do
      -- Easy/Standard: -X where X is half of your Memories (rounded down).
      -- Hard/Expert: -X where X is your Memories.
      memories <- getMemories iid
      pure $ toChaosTokenValue attrs Skull (memories `div` 2) memories
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ElectricNightmare where
  runMessage msg s@(ElectricNightmare attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      -- guide p6: each investigator with 3 or fewer Memories reads
      -- Desynchronization and adds the Desync weakness to their deck.
      checkDesynchronization
      pure s
    Setup -> runScenarioSetup ElectricNightmare attrs do
      setUsesGrid

      gather Set.ElectricNightmare
      gather Set.Endtimes
      gather Set.DarkPast
      gatherAndSetAside Set.TheBoogeyman

      -- Randomly select one version of act 1 (the other two are removed from
      -- the game, i.e. never added to the act deck).
      version <-
        sample
          $ Acts.publicSchool187V10
          :| [Acts.publicSchool187V20, Acts.publicSchool187V30]
      setActDeck [version, Acts.psychoanalysis, Acts.facingYourFears]
      setAgendaDeck [Agendas.figmentOfYourImagination, Agendas.it]

      -- Undefined Room locations + Entrance Hall (A Shimmer in the Wall) are set
      -- aside; they enter play via the act / School Grounds later.
      setAside
        [ Locations.cafeteria
        , Locations.classroomK2
        , Locations.gymnasium
        , Locations.biologyLab
        , Locations.library
        , Locations.entranceHall
        ]

      schoolGrounds <- placeInGrid (Pos 0 0) Locations.schoolGrounds
      startAt schoolGrounds
      placeAsset_ Assets.maja (AttachedToLocation schoolGrounds)

      -- Avatars (Reintegrated) and the K2-PS187 functionality assets set aside.
      setAside
        [ Stories.reintegrated_062
        , Stories.reintegrated_063
        , Stories.reintegrated_064
        , Stories.reintegrated_065
        , Assets.k2PS18725Functionality
        , Assets.k2PS18750Functionality
        , Assets.k2PS18775Functionality
        , Assets.k2PS187100Functionality
        ]

      -- If an investigator has been infected by the cybervirus, they begin the
      -- scenario with Cybervirus in their hand.
      infected <- getRecordSet HasBeenInfectedByTheCybervirus
      iids <- allInvestigators
      for_ iids \iid ->
        when (recorded (unInvestigatorId iid) `elem` infected) do
          card <- genCard Enemies.cybervirus
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
            [ Treacheries.reminiscencePledge
            , Treacheries.reminiscenceSecrets
            , Treacheries.reminiscenceCovenant
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
            1 -> Just Assets.k2PS18725Functionality
            2 -> Just Assets.k2PS18750Functionality
            3 -> Just Assets.k2PS18775Functionality
            _ -> Nothing
          addReminiscenceToken
          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        3 -> do
          record YouFullyRestoredTheSanityOfK2PS187
          offerK2Reward $ Just Assets.k2PS187100Functionality
          addReminiscenceToken
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        _ -> error "Invalid resolution"
      pure s
    _ -> ElectricNightmare <$> liftRunMessage msg attrs

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
