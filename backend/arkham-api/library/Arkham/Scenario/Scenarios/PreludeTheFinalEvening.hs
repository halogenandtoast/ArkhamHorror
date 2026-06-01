module Arkham.Scenario.Scenarios.PreludeTheFinalEvening (preludeTheFinalEvening) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Scenario.Import.Lifted

newtype PreludeTheFinalEvening = PreludeTheFinalEvening ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preludeTheFinalEvening :: Difficulty -> PreludeTheFinalEvening
preludeTheFinalEvening difficulty =
  scenarioWith
    PreludeTheFinalEvening
    "10679b"
    "The Vale"
    difficulty
    [ ".     triangle square"
    , "moon  triangle square"
    , "moon  diamond  star"
    , "heart diamond  star"
    , "heart circle   spade"
    , ".     circle   spade"
    ]
    $ (hasEncounterDeckL .~ False)
    . (referenceL .~ "10704")

instance HasChaosTokenValue PreludeTheFinalEvening where
  getChaosTokenValue iid tokenFace (PreludeTheFinalEvening attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PreludeTheFinalEvening where
  runMessage msg s@(PreludeTheFinalEvening attrs) = runQueueT $ campaignI18n $ scope "prelude4" $ case msg of
    PreScenarioSetup -> scope "intro" do
      hasPlan <- getHasRecord DrMarquezHasAPlan
      flavor do
        h "title"
        p "intro1"
        ul do
          li.validate hasPlan "proceedToIntro2"
          li.validate (not hasPlan) "skipToIntro5"
      if hasPlan
        then do
          motherRachelRel <- getRelationshipLevel MotherRachel
          flavor do
            h "title"
            p "intro2"
            ul do
              li.validate (motherRachelRel >= 3) "doneNothingWrongTruth"
              li.validate (motherRachelRel >= 3) "doneNothingWrongLie"
              li "sentencedToDeath"
          leadChooseOneM do
            when (motherRachelRel >= 3) do
              labeled' "doneNothingWrongTruth" intro3
              labeled' "doneNothingWrongLie" intro4
            labeled' "sentencedToDeath" intro5
        else intro5
      pure s
    Setup -> runScenarioSetup PreludeTheFinalEvening attrs do
      setup $ ul do
        li "gatherSets"
        li "dayThree"
        li "buildActAgenda"
        li.nested "placeLocations" do
          li "crossroadsAndOldMill"
          li "frenziedRevelers"
        li.nested "residents" do
          li "removeResidents"
          li "placeResidents"
          li "setOutOfPlay"
        li "startAt"
        li "placeDoom"
        unscoped $ li "readyToBegin"

      gather Set.TheFinalDay
      gather Set.DayOfTheFeast
      gather Set.TheVale
      gatherAndSetAside Set.Residents

      -- "Around the Table" serves as both the current act and the current
      -- agenda. We model it as the agenda (the doom track that leads to "The
      -- Sleep"); the act objective is added with the full Final Evening
      -- scenario. The special agenda "Lambs to the Slaughter" is set aside.
      setAgendaDeck [Agendas.aroundTheTable]
      setAside [Agendas.lambsToTheSlaughter]

      boardingHouse <- place Locations.boardingHouseDay
      theCrossroads <- place Locations.theCrossroadsEvening
      hemlockChapel <- place Locations.hemlockChapelDay
      placeAll [Locations.theAtwoodHouseDay]
      tadsStore <- place Locations.tadsGeneralStoreDay
      valeSchoolhouse <- place Locations.valeSchoolhouseDay
      theCommons <- place Locations.theCommonsDay
      theOldMill <- place Locations.theOldMillEvening

      n <- getPlayerCount
      createEnemyAt_ Enemies.frenziedReveler theCommons
      when (n >= 3) $ createEnemyAt_ Enemies.frenziedReveler hemlockChapel

      interrupted <- getHasRecord TheInvestigatorsInterruptedTheFeast
      if interrupted then startAt boardingHouse else startAt theCrossroads

      -- If North Point Mine was never surveyed, Leah Atwood's name is crossed out.
      northPointSurveyed <- getHasRecord (AreasSurveyed NorthPointMine)
      unless northPointSurveyed $ record LeahCrossedOut

      crossed0 <- getCrossedOutResidents
      let crossed =
            if northPointSurveyed || LeahAtwood `elem` crossed0
              then crossed0
              else LeahAtwood : crossed0

      -- Remove each crossed-out resident from the game.
      for_ crossed (obtainCard <=< fetchCard)

      -- Place each remaining resident. If their Relationship Level is at or below
      -- the listed threshold, they are placed on their enemy side instead.
      let placeResident resident loc threshold enemyCard assetCard =
            unless (resident `elem` crossed) do
              rel <- getRelationshipLevel resident
              if rel <= threshold
                then createEnemyAt_ enemyCard loc
                else createAssetAt_ assetCard (AtLocation loc)

      unless (MotherRachel `elem` crossed)
        $ createAssetAt_ Assets.motherRachelKindlyMatron (AtLocation theCrossroads)

      bertieEpiphany <- getHasRecord BertieHadAnEpiphany
      when bertieEpiphany $ createEnemyAt_ Enemies.bertieMusgrave theCrossroads

      -- If the investigators interrupted the Feast, Dr. Marquez joins the fray.
      -- TODO: search each investigator's deck and all out-of-play areas for Dr.
      -- Marquez, then put her into play under an investigator's control (she
      -- does not take up an ally slot for the remainder of this prelude).

      placeResident LeahAtwood hemlockChapel 2 Enemies.leahAtwood Assets.leahAtwoodTheValeCook
      placeResident
        SimeonAtwood
        valeSchoolhouse
        3
        Enemies.simeonAtwood
        Assets.simeonAtwoodDedicatedTroublemaker
      placeResident GideonMizrah theCommons 3 Enemies.gideonMizrah Assets.gideonMizrahSeasonedSailor
      placeResident JudithPark theOldMill 2 Enemies.judithPark Assets.judithParkTheMuscle
      placeResident TheoPeters tadsStore 2 Enemies.theoPeters Assets.theoPetersJackOfAllTrades

      placeDoomOnAgenda n
    _ -> PreludeTheFinalEvening <$> liftRunMessage msg attrs
   where
    intro3 = campaignI18n $ scope "prelude4" $ scope "intro" do
      flavor $ h "title" >> p "intro3"
      addChaosToken Cultist
      eachInvestigator \iid -> do
        cards <-
          select
            $ oneOf [inDeckOf iid, inHandOf NotForPlay iid, inDiscardOf iid]
            <> basic (cardIs Assets.drRosaMarquezBestInHerField)
        for_ cards (push . Msg.RemovePlayerCardFromGame True)
      record TheInvestigatorsBelieved
    intro4 = campaignI18n $ scope "prelude4" $ scope "intro" do
      flavor $ h "title" >> p "intro4"
      addChaosToken Cultist
      addChaosToken ElderThing
      record TheInvestigatorsLiedToMotherRachel
    intro5 = campaignI18n $ scope "prelude4" $ scope "intro" do
      flavor $ h "title" >> p "intro5"
      addChaosToken Tablet
      addChaosToken ElderThing
      record TheInvestigatorsInterruptedTheFeast
