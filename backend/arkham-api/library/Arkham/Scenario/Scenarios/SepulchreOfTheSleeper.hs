module Arkham.Scenario.Scenarios.SepulchreOfTheSleeper (sepulchreOfTheSleeper) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignStep (CampaignStep (EpilogueStep))
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers
import Arkham.Trait (Trait (Artifact))

newtype SepulchreOfTheSleeper = SepulchreOfTheSleeper ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

sepulchreOfTheSleeper :: Difficulty -> SepulchreOfTheSleeper
sepulchreOfTheSleeper difficulty =
  scenario
    SepulchreOfTheSleeper
    "11673"
    "Sepulchre of the Sleeper"
    difficulty
    [ "sigilCarvedAlcove1 sigilCarvedAlcove2 sigilCarvedAlcove3"
    , "sigilCarvedAlcove4 dreamersRest         sigilCarvedAlcove5"
    ]

instance HasChaosTokenValue SepulchreOfTheSleeper where
  getChaosTokenValue iid chaosTokenFace (SepulchreOfTheSleeper attrs) = case chaosTokenFace of
    Skull -> do
      -- "-X. X is the current Disturbance (to a maximum of 6) [-X. X is the current
      -- Disturbance]." Only easy/standard caps it.
      disturbance <- getDisturbance
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (min 6 disturbance) disturbance)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 6
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | "Before resolving any other resolution, if at least 1 investigator was
defeated: The defeated investigators read Investigator Defeat first."
-}
readInvestigatorDefeat :: (HasI18n, ReverseQueue m) => m ()
readInvestigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    survivors <- select $ not_ (mapOneOf InvestigatorWithId defeated)
    resolutionOnly defeated $ scope "investigatorDefeat" do
      setTitle "title"
      p "body"
      ul do
        li "drivenInsane"
        li.validate (notNull survivors) "otherResolution"
    for_ defeated drivenInsane

instance RunMessage SepulchreOfTheSleeper where
  runMessage msg s@(SepulchreOfTheSleeper attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup SepulchreOfTheSleeper attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "startAt"
        li "chooseExpeditionItem"
        li "artifacts"
        li "buildAgendaDeck"
        li "buildEncounterDeck"
        unscoped $ li "readyToBegin"

      additionalRules "disturbance"

      gather Set.SepulchreOfTheSleeper
      gather Set.Domination
      gather Set.Dreams
      gather Set.Rlyeh
      gather Set.StarSpawn
      gather Set.AncientEvils
      gather Set.StrikingFear

      -- "Build a special agenda deck using Beneath the City as the first agenda and
      -- Cthulhu Awakened as the second agenda. These replace the act and agenda
      -- decks." Cthulhu is the back of Beneath the City, so he only enters play when
      -- that agenda advances.
      setAgendaDeck [Agendas.beneathTheCity, Agendas.cthulhuAwakened]

      startAt =<< place Locations.dreamersRest
      placeGroup
        "sigilCarvedAlcove"
        [ Locations.sigilCarvedAlcoveStoryOfAmbition
        , Locations.sigilCarvedAlcoveStoryOfResilience
        , Locations.sigilCarvedAlcoveStoryOfInfinity
        , Locations.sigilCarvedAlcoveStoryOfDefiance
        , Locations.sigilCarvedAlcoveStoryOfTheVoyage
        ]

      -- "In player order, each player may choose 1 Item asset from the Expedition
      -- encounter set to begin in play under their control."
      eachInvestigator (`forInvestigator` Setup)
      -- The earned Artifacts are handed out afterwards, one at a time.
      doStep 1 Setup
    ForInvestigator iid Setup -> do
      chooseOneM iid do
        questionLabeled' "chooseExpeditionItem"
        labeled' "noExpeditionItem" nothing
        for_ expeditionItems \item ->
          cardLabeled item.cardCode $ handleTarget iid attrs (CardCodeTarget item.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- EncounterCard <$> genEncounterCard def
        createAssetAt_ card (InPlayArea iid)
      pure s
    DoStep 1 Setup -> do
      -- "Gather all earned Artifact assets and put each of them into play under an
      -- investigator's control, divided as evenly as possible." One message per
      -- artifact, so each choice sees the ones already handed out.
      artifacts <- getEarnedArtifacts
      for_ artifacts \def -> push $ ForTarget (CardCodeTarget def.cardCode) (DoStep 1 Setup)
      pure s
    ForTarget (CardCodeTarget cardCode) (DoStep 1 Setup) -> do
      for_ (lookupCardDef cardCode) \def -> do
        investigators <- select Anyone
        counts <-
          for investigators \iid ->
            (iid,)
              <$> selectCount (AssetWithTrait Artifact <> AssetControlledBy (InvestigatorWithId iid))
        -- "As evenly as possible": only investigators tied for the fewest Artifacts
        -- so far are eligible, and the players pick between them.
        unless (null counts) do
          let fewest = minimumEx $ map snd counts
          card <- EncounterCard <$> genEncounterCard def
          lead <- getLead
          chooseOrRunOneM lead do
            questionLabeled' "chooseArtifactInvestigator"
            targets [iid | (iid, n) <- counts, n == fewest] (createAssetAt_ card . InPlayArea)
      pure s
    ScenarioSpecific "increaseDisturbance" _ -> do
      -- "Place 1 resource on the scenario reference card, as Disturbance."
      placeTokens attrs ScenarioTarget #resource 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case chaosTokenFace token of
        -- "If you fail, draw the top card of the encounter deck. If you fail by 3
        -- or more, that card gains peril and surge." Failing by 3+ pulls the card
        -- aside so the keywords can be attached before it resolves.
        Cultist ->
          if n >= 3
            then drawEncounterCardsEdit iid Cultist 1 (setTarget token)
            else drawEncounterCard iid Cultist
        -- "For each point you fail by, you must either take 1 horror or discard 1
        -- card from your hand." One choice at a time, so each sees the hand as it
        -- stands; once it is empty the horror is all that is left.
        Tablet -> replicateM_ n $ forInvestigator iid (ScenarioSpecific "tabletPenalty" Null)
        _ -> pure ()
      pure s
    ForInvestigator iid (ScenarioSpecific "tabletPenalty" _) -> do
      hasCards <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid Tablet 1
        when hasCards
          $ countVar 1
          $ labeled' "discardCardsFromHand"
          $ chooseAndDiscardCard iid Tablet
      pure s
    DrewCards iid drewCards | Just (ChaosTokenTarget (chaosTokenFace -> Cultist)) <- drewCards.target -> do
      for_ drewCards.cards \card -> do
        cardResolutionModifiers card Cultist card [AddKeyword Keyword.Peril, AddKeyword Keyword.Surge]
        drawCardFrom iid Deck.EncounterDeck (toCard card)
      pure s
    -- {elderThing}: "Reveal another token."
    ResolveChaosToken _ ElderThing iid -> do
      drawAnotherChaosToken iid
      pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        Resolution 1 -> do
          readInvestigatorDefeat
          record TheInvestigatorsHaltedCthulhusAwakening
          -- "Each investigator earns 10 bonus experience, as they prevented an
          -- ancient evil from rising to terrorize all of the earth once again."
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 10
          eachInvestigator (`sufferMentalTrauma` 2)
          -- "The investigators win the campaign! Proceed to Epilogue."
          endOfScenarioThen EpilogueStep
        NoResolution -> do
          readInvestigatorDefeat
          -- "If there are no surviving investigators, the investigators lose the
          -- campaign." No resolution is only reached once every investigator has
          -- been defeated or driven insane by Your Inevitable Doom.
          gameOver
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> SepulchreOfTheSleeper <$> liftRunMessage msg attrs
