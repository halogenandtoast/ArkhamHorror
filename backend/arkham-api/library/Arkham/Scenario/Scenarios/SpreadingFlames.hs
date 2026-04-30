module Arkham.Scenario.Scenarios.SpreadingFlames (spreadingFlames) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.BrethrenOfAsh.Import
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.I18n (ikey)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher (cardIs)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.SpreadingFlames.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype SpreadingFlames = SpreadingFlames ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

spreadingFlames :: Difficulty -> SpreadingFlames
spreadingFlames difficulty =
  scenario
    SpreadingFlames
    "12105"
    "Spreading Flames"
    difficulty
    [ ".                orneLibrary     ."
    , "dormitories      miskatonicQuad  warrenObservatory"
    , "yourFriendsRoom  scienceHall     ."
    ]

instance HasChaosTokenValue SpreadingFlames where
  getChaosTokenValue iid chaosTokenFace (SpreadingFlames attrs) = case chaosTokenFace of
    Skull -> do
      actStep <- getCurrentActStep
      pure $ toChaosTokenValue attrs Skull actStep (actStep + 1)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage SpreadingFlames where
  runMessage msg s@(SpreadingFlames attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup SpreadingFlames attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "startAt"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.SpreadingFlames
      gather Set.AshenPilgrims
      gather Set.Bystanders
      gather Set.CosmicEvils
      gather Set.EldritchLore
      gather Set.Hallucinations
      gather Set.Fire1
      gather Set.MadScience
      gather Set.MiskatonicUniversity

      setAside
        [ Enemies.servantOfFlameRagingFury
        , Assets.drHenryArmitage_c2026
        , --- Locations
          Locations.miskatonicQuad_c2026
        , Locations.dormitories_c2026
        , Locations.scienceHall
        , Locations.warrenObservatory_c2026
        , Locations.orneLibrary_c2026
        ]
      setAside =<< amongGathered (cardIs Treacheries.fire1)
      startAt =<< place Locations.yourFriendsRoom

      setAgendaDeck [Agendas.pastCurfew, Agendas.litUp, Agendas.wildFlames]
      setActDeck
        [Acts.whereTheresSmoke, Acts.escapeTheDorms, Acts.searchingForDrArmitage, Acts.blazeOfGlory]
    ResolveChaosToken _ Tablet iid -> do
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Tablet -> assignDamage iid Tablet 1
        ElderThing | isEasyStandard attrs && n >= 2 -> do
          findTopOfDiscard (cardIs Treacheries.fire1) >>= traverse_ (drawCardFrom iid Deck.EncounterDiscard)
        ElderThing | isHardExpert attrs -> do
          findTopOfDiscard (cardIs Treacheries.fire1) >>= traverse_ (drawCardFrom iid Deck.EncounterDiscard)
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          record MiskatonicUniversityBurned
          eachInvestigator (`sufferMentalTrauma` 1)
          resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 2
          endOfScenario
        Resolution 1 -> do
          record InvestigatorsDefeatedTheirMaskedPursuer
          resolution "resolution1"
          addCampaignCardToDeckChoice_ Assets.drHenryArmitage_c2026
          resolutionWithXpAndChooseOne "resolution1" (allGainXpWithBonus' attrs $ toBonus "bonus" 3) do
            labeled' "r2" $ push $ ScenarioResolution $ Resolution 2
            labeled' "r3" $ push $ ScenarioResolution $ Resolution 3
        Resolution 2 -> do
          record InvestigatorsSavedMiskatonicUniversity
          resolution "resolution2"
          eachInvestigator \iid -> do
            gainXp iid attrs (ikey "xp.resolution2") 1
            sufferPhysicalTrauma iid 1
          endOfScenario
        Resolution 3 -> do
          record MiskatonicUniversityBurned
          resolution "resolution3"
          eachInvestigator (`sufferMentalTrauma` 1)
          endOfScenario
        other -> throwIO $ UnknownResolution other

      pure s
    _ -> SpreadingFlames <$> liftRunMessage msg attrs
