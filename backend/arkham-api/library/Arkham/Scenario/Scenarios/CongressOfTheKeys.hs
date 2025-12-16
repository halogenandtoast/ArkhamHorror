module Arkham.Scenario.Scenarios.CongressOfTheKeys (congressOfTheKeys) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.CongressOfTheKeys.Helpers

newtype CongressOfTheKeys = CongressOfTheKeys ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congressOfTheKeys :: Difficulty -> CongressOfTheKeys
congressOfTheKeys difficulty =
  scenario
    CongressOfTheKeys
    "09694"
    "Congress of the Keys"
    difficulty
    []

instance HasChaosTokenValue CongressOfTheKeys where
  getChaosTokenValue iid tokenFace (CongressOfTheKeys attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage CongressOfTheKeys where
  runMessage msg s@(CongressOfTheKeys attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      t <- getTime
      flavor do
        setTitle "title"
        p "body"
        ul do
          li.validate (t >= 35) "option1"
          li.validate (t < 35) "option2"
      flavor $ setTitle "title" >> p (if t >= 35 then "intro1" else "intro2")
      doStep 1 PreScenarioSetup
      pure s
    DoStep 1 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "theTrial1"
      theCellAidedTheKnight <- getHasRecord TheCellAidedTheKnight
      theCellFailedToFendOffTheBeast <- getHasRecord TheCellFailedToFendOffTheBeast
      haventSeenTheLastOfTheClaretKnight <- getHasRecord YouHaventSeenTheLastOfTheClaretKnight
      theDogsAreAtWar <- getHasRecord TheDogsAreAtWar
      let
        nay1
          | theCellFailedToFendOffTheBeast = 0
          | haventSeenTheLastOfTheClaretKnight = 0
          | theDogsAreAtWar = 0
          | otherwise = 1
        yay1
          | theCellFailedToFendOffTheBeast = 1
          | haventSeenTheLastOfTheClaretKnight = 2
          | theDogsAreAtWar = 0
          | otherwise = 1

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay1 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay1 $ p "yay.count"

        compose.red.bordered do
          p.validate theCellAidedTheKnight "theCellAidedTheKnight"
          hr
          p.validate theCellFailedToFendOffTheBeast "theCellFailedToFendOffTheBeast"
          hr
          p.validate haventSeenTheLastOfTheClaretKnight "haventSeenTheLastOfTheClaretKnight"
          hr
          p.validate theDogsAreAtWar "theDogsAreAtWar"
          hr
          p.validate
            ( not
                $ theCellAidedTheKnight
                || theCellFailedToFendOffTheBeast
                || haventSeenTheLastOfTheClaretKnight
                || theDogsAreAtWar
            )
            "dogsOfWarSkipped"

      unless
        ( theCellAidedTheKnight
            || theCellFailedToFendOffTheBeast
            || haventSeenTheLastOfTheClaretKnight
            || theDogsAreAtWar
        )
        do
          lightBearer <-
            sampleOneOf
              ( Enemies.theClaretKnightHoldsYouInContempt
              , Enemies.theBeastInACowlOfCrimsonLeavingATrailOfDestruction
              )
          setBearer Keys.theLightOfPharos (keyWithEnemy lightBearer)

      eceTrustsTheCell <- getHasRecord EceTrustsTheCell
      eceDoesNotTrustTheCell <- getHasRecord EceDoesNotTrustTheCell

      let
        nay2 =
          nay1 + case () of
            _
              | eceTrustsTheCell -> 1
              | eceDoesNotTrustTheCell -> 0
              | otherwise -> 1
        yay2 = yay1

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay2 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay2 $ p "yay.count"

        compose.red.bordered do
          p.validate eceTrustsTheCell "eceTrustsTheCell"
          hr
          p.validate eceDoesNotTrustTheCell "eceDoesNotTrustTheCell"
          hr
          p.validate (not $ eceTrustsTheCell || eceDoesNotTrustTheCell) "dealingsInTheDarkSkipped"

      pure s
    Setup -> runScenarioSetup CongressOfTheKeys attrs do
      gather Set.CongressOfTheKeys
    _ -> CongressOfTheKeys <$> liftRunMessage msg attrs
