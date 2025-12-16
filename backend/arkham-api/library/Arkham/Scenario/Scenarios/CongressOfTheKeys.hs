module Arkham.Scenario.Scenarios.CongressOfTheKeys (congressOfTheKeys) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card.CardCode
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign
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
      youHaventSeenTheLastOfTheClaretKnight <- getHasRecord YouHaventSeenTheLastOfTheClaretKnight
      theDogsAreAtWar <- getHasRecord TheDogsAreAtWar
      let
        nay1
          | theCellFailedToFendOffTheBeast = 0
          | youHaventSeenTheLastOfTheClaretKnight = 0
          | theDogsAreAtWar = 0
          | otherwise = 1
        yay1
          | theCellFailedToFendOffTheBeast = 1
          | youHaventSeenTheLastOfTheClaretKnight = 2
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
          p.validate youHaventSeenTheLastOfTheClaretKnight "youHaventSeenTheLastOfTheClaretKnight"
          hr
          p.validate theDogsAreAtWar "theDogsAreAtWar"
          hr
          p.validate
            ( not
                $ theCellAidedTheKnight
                || theCellFailedToFendOffTheBeast
                || youHaventSeenTheLastOfTheClaretKnight
                || theDogsAreAtWar
            )
            "dogsOfWarSkipped"

      unless
        ( theCellAidedTheKnight
            || theCellFailedToFendOffTheBeast
            || youHaventSeenTheLastOfTheClaretKnight
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

      youHaventSeenTheLastOfAmaranth <- getHasRecord YouHaventSeenTheLastOfAmaranth
      theLoversAreReunited <- getHasRecord TheLoversAreReunited
      amaranthHasLeftTheCoterie <- getHasRecord TheRedCoterieWasDestroyedFromWithin

      unless (youHaventSeenTheLastOfAmaranth || theLoversAreReunited || amaranthHasLeftTheCoterie) do
        setBearer Keys.theLastBlossom (keyWithEnemy Enemies.amaranthScarletScorn)

      let
        nay3 = nay2
        yay3 =
          yay2 + case () of
            _
              | youHaventSeenTheLastOfAmaranth -> 1
              | theLoversAreReunited -> 2
              | amaranthHasLeftTheCoterie -> 0
              | otherwise -> 1

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay3 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay3 $ p "yay.count"

        compose.red.bordered do
          p.validate youHaventSeenTheLastOfAmaranth "youHaventSeenTheLastOfAmaranth"
          hr
          p.validate theLoversAreReunited "theLoversAreReunited"
          hr
          p.validate amaranthHasLeftTheCoterie "amaranthHasLeftTheCoterie"
          hr
          p.validate
            ( not
                $ youHaventSeenTheLastOfAmaranth
                || theLoversAreReunited
                || amaranthHasLeftTheCoterie
            )
            "deadHeatSkipped"

      youHaventSeenTheLastOfThorne <- getHasRecord YouHaventSeenTheLastOfThorne
      theCellMadeADealWithThorne <- getHasRecord TheCellMadeADealWithThorne
      thorneDisappeared <- getHasRecord ThorneDisappeared

      let
        nay4 = nay3 + if theCellMadeADealWithThorne then 1 else 0
        yay4 = yay3 + if youHaventSeenTheLastOfThorne then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay4 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay4 $ p "yay.count"

        compose.red.bordered do
          p.validate youHaventSeenTheLastOfThorne "youHaventSeenTheLastOfThorne"
          hr
          p.validate theCellMadeADealWithThorne "theCellMadeADealWithThorne"
          hr
          p.validate thorneDisappeared "thorneDisappeared"
          hr
          p.validate
            ( not
                $ youHaventSeenTheLastOfThorne
                || theCellMadeADealWithThorne
                || thorneDisappeared
            )
            "onThinIceSkipped"

      unless (youHaventSeenTheLastOfThorne || theCellMadeADealWithThorne || thorneDisappeared) do
        setBearer Keys.theSableGlass (keyWithEnemy Enemies.thorneOpenToNegotiation)

      alikiIsOnYourSide <- getHasRecord AlikiIsOnYourSide
      youHaventSeenTheLastOfAlikiZoniUperetria <- getHasRecord YouHaventSeenTheLastOfAlikiZoniUperetria

      let
        nay5 = nay4 + if alikiIsOnYourSide then 1 else 0
        yay5 = yay4 + if youHaventSeenTheLastOfAlikiZoniUperetria then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay5 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay5 $ p "yay.count"

        compose.red.bordered do
          p.validate alikiIsOnYourSide "alikiIsOnYourSide"
          hr
          p.validate youHaventSeenTheLastOfAlikiZoniUperetria "youHaventSeenTheLastOfAlikiZoniUperetria"
          hr
          p.validate
            (not $ alikiIsOnYourSide || youHaventSeenTheLastOfAlikiZoniUperetria)
            "withoutATraceSkipped"

      -- desiIsInYourDebt <- getHasRecord DesiIsInYourDebt
      mdesi <- stored @CardCode "desidarioVersion"
      let desiIsGood = mdesi == Just "09607"
      let desiIsBad = mdesi == Just "09606"
      youHaventSeenTheLastOfDesiderioDelgadoAlvarez <-
        getHasRecord YouHaventSeenTheLastOfDesiderioDelgadoAlvarez
      let skippedDancingMad = not $ desiIsGood || desiIsBad || youHaventSeenTheLastOfDesiderioDelgadoAlvarez

      let
        nay6 = nay5 + if desiIsGood then 1 else 0
        yay6 = yay5 + if skippedDancingMad then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay6 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay6 $ p "yay.count"

        compose.red.bordered do
          p "desi"
          hr
          p.validate desiIsGood "desiIsGood"
          hr
          p.validate (desiIsBad || youHaventSeenTheLastOfDesiderioDelgadoAlvarez) "desiIsBad"
          hr
          p.validate skippedDancingMad "dancingMadSkipped"

      when skippedDancingMad do
        setBearer Keys.theMirroringBlade (keyWithEnemy Enemies.desiderioDelgadoAlvarezRedInHisLedger)

      theCellMeddledInAbarransAffairs <- getHasRecord TheCellMeddledInAbarransAffairs

      let
        nay7 = nay6
        yay7 = yay6 + if theCellMeddledInAbarransAffairs then 1 else 0

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay7 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay7 $ p "yay.count"

        compose.red.bordered do
          p.validate theCellMeddledInAbarransAffairs "theCellMeddledInAbarransAffairs"
          hr
          p.validate (not theCellMeddledInAbarransAffairs) "fortuneAndFollySkipped"

      theCellKnowsTheTrueNatureOfTheCoterie <- getHasRecord TheCellKnowsTheTrueNatureOfTheCoterie
      let eerilySilentCount =
            count
              id
              [ thorneDisappeared
              , youHaventSeenTheLastOfAlikiZoniUperetria
              , youHaventSeenTheLastOfDesiderioDelgadoAlvarez || desiIsBad
              ]
      let eerilySilent = 1 + eerilySilentCount >= 3
      let continueTheTrial = not $ theCellKnowsTheTrueNatureOfTheCoterie || eerilySilent

      flavor do
        cols do
          compose do
            p "nay.title"
            countVar nay7 $ p "nay.count"
          compose do
            p "yay.title"
            countVar yay7 $ p "yay.count"

        compose.red.bordered do
          p "noMatterWhat"
          p.right.validate theCellKnowsTheTrueNatureOfTheCoterie "theCellKnowsTheTrueNatureOfTheCoterie"
          p.right.validate eerilySilent "eerilySilent"
          p.right.validate continueTheTrial "continueTheTrial"

      if
        | theCellKnowsTheTrueNatureOfTheCoterie -> doStep 6 PreScenarioSetup
        | eerilySilent -> doStep 7 PreScenarioSetup
        | otherwise -> do
            pure ()
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "theTrial6"
      pure s
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "theTrial7"
      pure s
    Setup -> runScenarioSetup CongressOfTheKeys attrs do
      gather Set.CongressOfTheKeys
    _ -> CongressOfTheKeys <$> liftRunMessage msg attrs
