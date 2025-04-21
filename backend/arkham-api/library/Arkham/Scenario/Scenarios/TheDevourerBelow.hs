module Arkham.Scenario.Scenarios.TheDevourerBelow where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.Campaigns.NightOfTheZealot.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message (pushWhenM)
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message hiding (chooseOrRunOne, story)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (
  assignDamageAndHorror,
  chooseOrRunOne,
  findAndDrawEncounterCard,
  placeLocationCard,
  story,
 )
import Arkham.Scenarios.TheDevourerBelow.Helpers
import Arkham.Trait hiding (Cultist, ElderThing)

newtype TheDevourerBelow = TheDevourerBelow ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDevourerBelow :: Difficulty -> TheDevourerBelow
theDevourerBelow difficulty =
  scenario
    TheDevourerBelow
    "01142"
    "The Devourer Below"
    difficulty
    [ "woods1     .     woods2"
    , "woods1 mainPath woods2"
    , "woods3 mainPath woods4"
    , "woods3 ritualSite woods4"
    , "   .   ritualSite   .  "
    ]

instance HasChaosTokenValue TheDevourerBelow where
  getChaosTokenValue iid chaosTokenFace (TheDevourerBelow attrs) = case chaosTokenFace of
    Skull -> do
      monsterCount <- selectCount $ EnemyWithTrait Monster
      pure $ toChaosTokenValue attrs Skull monsterCount 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 5 7
    otherFace -> getChaosTokenValue iid otherFace attrs

actDeck :: [CardDef]
actDeck =
  [Acts.investigatingTheTrail, Acts.intoTheDarkness, Acts.disruptingTheRitual]

agendaDeck :: [CardDef]
agendaDeck =
  [Agendas.theArkhamWoods, Agendas.theRitualBegins, Agendas.vengeanceAwaits]

instance RunMessage TheDevourerBelow where
  runMessage msg s@(TheDevourerBelow attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      push $ SetChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
      pure s
    Setup -> runScenarioSetup TheDevourerBelow attrs do
      cultistsWhoGotAway <- getRecordedCardCodes CultistsWhoGotAway
      pastMidnight <- getHasRecord ItIsPastMidnight
      ghoulPriestIsStillAlive <- getHasRecord GhoulPriestIsStillAlive
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setOutOfPlay"
        li "randomSet"
        scope "cultistsWhoGotAway" $ li.nested "instructions" do
          li.validate (null cultistsWhoGotAway) "zeroNames"
          li.validate (length cultistsWhoGotAway `elem` [1, 2]) "oneOrTwoNames"
          li.validate (length cultistsWhoGotAway `elem` [3, 4]) "threeOrFourNames"
          li.validate (length cultistsWhoGotAway `elem` [5, 6]) "fiveOrSixNames"
        unscoped $ withVars ["token" .= String "elderThing"] $ li "addToken"
        li.validate pastMidnight "pastMidnight"
        li.validate ghoulPriestIsStillAlive "ghoulPriest"
      gather EncounterSet.TheDevourerBelow
      gather EncounterSet.AncientEvils
      gather EncounterSet.StrikingFear
      gather EncounterSet.Ghouls
      gather EncounterSet.DarkCult
      gatherOneOf
        $ EncounterSet.AgentsOfYogSothoth
        :| [EncounterSet.AgentsOfShubNiggurath, EncounterSet.AgentsOfCthulhu, EncounterSet.AgentsOfHastur]

      setAside [Locations.ritualSite, Enemies.umordhoth]
      when ghoulPriestIsStillAlive $ addToEncounterDeck (Only Enemies.ghoulPriest)

      setActDeck actDeck
      setAgendaDeck agendaDeck
      addChaosToken ElderThing

      startAt =<< place Locations.mainPath
      placeGroupChooseN 4 "woods"
        $ Locations.arkhamWoodsUnhallowedGround
        :| [ Locations.arkhamWoodsTwistingPaths
           , Locations.arkhamWoodsOldHouse
           , Locations.arkhamWoodsCliffside
           , Locations.arkhamWoodsTangledThicket
           , Locations.arkhamWoodsQuietGlade
           ]

      let placeDoomAmount = (length cultistsWhoGotAway + 1) `div` 2
      pushWhen (placeDoomAmount > 0) $ PlaceDoomOnAgenda placeDoomAmount CanNotAdvance
      when pastMidnight $ twice $ allRandomDiscard attrs AnyCard
    ResolveChaosToken _ Cultist iid -> do
      let doom = if isEasyStandard attrs then 1 else 2
      enemies <- select $ NearestEnemyTo iid AnyEnemy
      chooseOrRunOneM iid $ targets enemies \x -> placeTokens Cultist x #doom doom
      pure s
    ResolveChaosToken _ Tablet iid -> do
      let horror = byDifficulty attrs 0 1
      whenM (selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Monster)
        $ assignDamageAndHorror iid (ChaosTokenEffectSource Tablet) 1 horror
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      pushWhenM (selectAny $ EnemyWithTrait AncientOne) $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Skull)) _ _ | isHardExpert attrs -> do
      findAndDrawEncounterCard iid $ CardWithType EnemyType <> CardWithTrait Monster
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          story $ i18n "noResolution"
          record ArkhamSuccumbedToUmordhothsTerribleVengeance
        Resolution 1 -> do
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" 5
          story $ withVars ["xp" .= xp] $ i18n "resolution1"
          record TheRitualToSummonUmordhothWasBroken
        Resolution 2 -> do
          xp <- allGainXpWithBonus' attrs $ toBonus "bonus" 10
          story $ withVars ["xp" .= xp] $ i18n "resolution2"
          record TheInvestigatorsRepelledUmordoth
        Resolution 3 -> do
          xp <- allGainXp' attrs
          story $ withVars ["xp" .= xp] $ i18n "resolution3"
          record TheInvestigatorsSacrificedLitaChantlerToUmordhoth
        _ -> error "Invalid resolution"
      endOfScenario
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ do
        case option of
          AddLitaChantler -> do
            investigators <- allInvestigators
            forceAddCampaignCardToDeckChoice investigators ShuffleIn Assets.litaChantler
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheDevourerBelow <$> liftRunMessage msg attrs
