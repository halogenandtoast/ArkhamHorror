module Arkham.Scenario.Scenarios.ToTheForbiddenPeaks (toTheForbiddenPeaks) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Log (whenHasRecord)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Text
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype ToTheForbiddenPeaks = ToTheForbiddenPeaks ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Note: The Campaign Guide refers to it as level 0, but it is easier for us to
-- start with level 1 since we can use the existing helper
toTheForbiddenPeaks :: Difficulty -> ToTheForbiddenPeaks
toTheForbiddenPeaks difficulty =
  scenario
    ToTheForbiddenPeaks
    "08596"
    "To the Forbidden Peaks"
    difficulty
    [ ".      .      .      .      .      theSummit"
    , ".      .      .      .      level5 theSummit"
    , ".      .      .      level4 level5 ."
    , ".      .      level3 level4 .      ."
    , ".      level2 level3 .      .      ."
    , "level1 level2 .      .      .      ."
    , "level1 .      .      .      .      ."
    ]

instance HasChaosTokenValue ToTheForbiddenPeaks where
  getChaosTokenValue iid tokenFace (ToTheForbiddenPeaks attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ToTheForbiddenPeaks where
  runMessage msg s@(ToTheForbiddenPeaks attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"

      eliyahIsAlive <- getPartnerIsAlive Assets.eliyahAshevakDogHandler
      woodenSledgeRecovered <- hasSupply WoodenSledge
      blueStory
        $ validateEntry (eliyahIsAlive && woodenSledgeRecovered) "eliyah.alive"
        <> validateEntry (not $ eliyahIsAlive && woodenSledgeRecovered) "eliyah.otherwise"

      unless (eliyahIsAlive && woodenSledgeRecovered) do
        whenM hasRemainingFrostTokens $ addChaosToken #frost

      claypoolIsAlive <- getPartnerIsAlive Assets.averyClaypoolAntarcticGuide
      blueStory
        $ validateEntry claypoolIsAlive "claypool.alive"
        <> validateEntry (not claypoolIsAlive) "claypool.otherwise"

      lead <- getLead

      unless claypoolIsAlive do
        chooseOneM lead do
          whenM hasRemainingFrostTokens do
            labeled "Add 1 {frost} token to the chaos bag" $ addChaosToken #frost
          labeled "Each investigator suffers 1 physical trauma." $ eachInvestigator (`sufferPhysicalTrauma` 1)

      takadaIsAlive <- getPartnerIsAlive Assets.takadaHirokoAeroplaneMechanic
      blueStory
        $ validateEntry takadaIsAlive "takada.alive"
        <> validateEntry (not takadaIsAlive) "takada.otherwise"

      unless takadaIsAlive do
        chooseOneM lead do
          whenM hasRemainingFrostTokens do
            labeled "Add 1 {frost} token to the chaos bag" $ addChaosToken #frost
          labeled "Each investigator suffers 1 mental trauma." $ eachInvestigator (`sufferMentalTrauma` 1)

      scoutedTheMountainPass <- getHasRecord TheInvestigatorsScoutedTheMountainPass
      blueStory
        $ validateEntry scoutedTheMountainPass "theMountainPass.scouted"
        <> validateEntry (not scoutedTheMountainPass) "theMountainPass.otherwise"

      story $ i18nWithTitle "intro2"
      eachInvestigator (`forInvestigator` PreScenarioSetup)
      pure s
    ForInvestigator iid PreScenarioSetup -> do
      partners <- getRemainingPartners
      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          labeled "Do not take a partner" nothing
          for_ partners \partner -> do
            inPlay <- selectAny $ assetIs partner.cardCode
            unless inPlay do
              cardLabeled partner.cardCode $ handleTarget iid ScenarioSource (CardCodeTarget partner.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        assetId <- createAssetAt card (InPlayArea iid)
        partner <- getPartner cardCode
        pushWhen (partner.damage > 0) $ Msg.PlaceDamage CampaignSource (toTarget assetId) partner.damage
        pushWhen (partner.horror > 0) $ Msg.PlaceHorror CampaignSource (toTarget assetId) partner.horror
      pure s
    Setup -> runScenarioSetup ToTheForbiddenPeaks attrs do
      gather Set.ToTheForbiddenPeaks
      gather Set.DeadlyWeather
      gather Set.ElderThings
      gather Set.HazardsOfAntarctica
      gather Set.NamelessHorrors
      gather Set.Tekelili

      setAgendaDeck [Agendas.forbiddenPeaks, Agendas.terrorDescends]
      setActDeck [Acts.ascendTheMountain]

      mountainSides <-
        placeGroupCapture "level"
          =<< fmap (map toCardDef . take 5)
          . shuffle
          =<< amongGathered (#location <> CardWithTitle "Mountainside")

      for_ (nonEmpty mountainSides) \(level0 :| _) -> do
        whenHasRecord TheInvestigatorsScoutedTheMountainPass do
          setupModifier ScenarioSource level0 ReduceStartingCluesByHalf
        startAt level0

      place_ Locations.theSummit

      addChaosToken ElderThing
      setAside [Enemies.terrorOfTheStarsGuardianOfForbiddenPeaks]

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()

      addTekeliliDeck
    _ -> ToTheForbiddenPeaks <$> liftRunMessage msg attrs
