module Arkham.Scenario.Scenarios.ReturnToUndimensionedAndUnseen (returnToUndimensionedAndUnseen) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.UndimensionedAndUnseen
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Trait (Trait (Injury, Madness, Pact))

newtype ReturnToUndimensionedAndUnseen = ReturnToUndimensionedAndUnseen UndimensionedAndUnseen
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToUndimensionedAndUnseen :: Difficulty -> ReturnToUndimensionedAndUnseen
returnToUndimensionedAndUnseen difficulty =
  scenario
    (ReturnToUndimensionedAndUnseen . UndimensionedAndUnseen)
    "51041"
    "Return to Undimensioned and Unseen"
    difficulty
    [ ". blastedHeath devilsHopYard"
    , ". blastedHeath devilsHopYard"
    , "dunwichVillage tenAcreMeadow ."
    , "dunwichVillage tenAcreMeadow whateleyRuins"
    , ". coldSpringGlen whateleyRuins"
    , ". coldSpringGlen ."
    ]

instance RunMessage ReturnToUndimensionedAndUnseen where
  runMessage msg (ReturnToUndimensionedAndUnseen undimensionedAndUnseen'@(UndimensionedAndUnseen attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToUndimensionedAndUnseen . UndimensionedAndUnseen) attrs do
      standalone <- getIsStandalone
      sacrificedToYogSothoth <-
        if standalone
          then pure 3
          else length <$> getRecordSet SacrificedToYogSothoth

      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li.nested "sacrificedToYogSothoth.instructions" do
            li.validate (sacrificedToYogSothoth >= 4) "sacrificedToYogSothoth.fourOrMore"
            li.validate (sacrificedToYogSothoth == 3) "sacrificedToYogSothoth.exactlyThree"
            li.validate (sacrificedToYogSothoth == 2) "sacrificedToYogSothoth.exactlyTwo"
            li.validate (sacrificedToYogSothoth <= 1) "sacrificedToYogSothoth.oneOrFewer"
          li "setAside"
          li "powderOfIbnGhazi"
          li "randomBasicWeakness"
          unscoped $ li "shuffleRemainder"

      scope "choosingARandomLocation" $ flavor do
        setTitle "title"
        p "body"

      gather Set.ReturnToUndimensionedAndUnseen
      gather Set.UndimensionedAndUnseen
      gather Set.Whippoorwills
      gather Set.BeastThralls
      gather Set.Dunwich
      gather Set.ErraticFear

      removeEvery [Enemies.broodOfYogSothoth]

      tenAcreMeadow <- sample2 Locations.tenAcreMeadow_246 Locations.tenAcreMeadow_247
      whateleyRuins <- sample2 Locations.whateleyRuins_250 Locations.whateleyRuins_251
      devilsHopYard <- sample2 Locations.devilsHopYard_252 Locations.devilsHopYard_253

      startAt =<< placeOneOf (Locations.dunwichVillage_242, Locations.dunwichVillage_243)
      coldSpringGlen <- placeOneOf (Locations.coldSpringGlen_244, Locations.coldSpringGlen_245)
      blastedHeath <- placeOneOf (Locations.blastedHeath_248, Locations.blastedHeath_249)
      placeAll [tenAcreMeadow, whateleyRuins, devilsHopYard]

      setAside $ replicate 4 Assets.esotericFormula

      broods <- shuffle =<< amongGathered "Brood of Yog-Sothoth"
      removeCards broods

      case sacrificedToYogSothoth of
        2 -> do
          let (coldGlenBroods, remainingBroods) = splitAt 1 broods
          for_ coldGlenBroods (`createEnemyAt_` coldSpringGlen)
          setAside $ take 3 remainingBroods
        3 -> do
          let (coldGlenBroods, remainingBroods) = splitAt 1 broods
          for_ coldGlenBroods (`createEnemyAt_` coldSpringGlen)
          setAside $ take 2 remainingBroods
        x ->
          if x <= 2
            then do
              let (coldGlenBroods, remainingBroods) = splitAt 1 broods
              let (blastedHeathBroods, remainingBroods') = splitAt 1 remainingBroods
              for_ coldGlenBroods (`createEnemyAt_` coldSpringGlen)
              for_ blastedHeathBroods (`createEnemyAt_` blastedHeath)
              setAside $ take 3 remainingBroods'
            else setAside $ take 2 broods

      eachInvestigator \iid -> do
        mcard <- findCardMatch Assets.powderOfIbnGhazi <$> field InvestigatorDeck iid
        for_ mcard $ \card -> do
          chooseOneM iid do
            labeled "Play Powder of Ibn-Ghazi" $ putCardIntoPlay iid card
            labeled "Do no play Powder of Ibn-Ghazi" nothing
        unlessStandalone do
          searchCollectionForRandom iid attrs
            $ BasicWeaknessCard
            <> hasAnyTrait [Madness, Injury, Pact]

      setAgendaDeck [Agendas.rampagingCreatures, Agendas.bidingItsTime, Agendas.horrorsUnleashed]
      setActDeck [Acts.saracenicScript, Acts.theyMustBeDestroyed]
      addAdditionalReferences ["51041b"]
    _ -> ReturnToUndimensionedAndUnseen <$> liftRunMessage msg undimensionedAndUnseen'
