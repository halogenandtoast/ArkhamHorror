module Arkham.Scenario.Scenarios.TheBlobThatAteEverything (theBlobThatAteEverything) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.DamageEffect (nonAttack)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (getModifiedKeywords)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.Query (allInvestigators, getPlayerCount)
import Arkham.Helpers.SkillTest (getCommittedCards, getSkillTestAction, getSkillTestTargetedEnemy, withSkillTest)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement (Placement (Global))
import Arkham.Projection
import Arkham.Resolution
import Arkham.Card (Card)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (difficultyL)
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Data.Aeson.Key qualified as Key

newtype TheBlobThatAteEverything = TheBlobThatAteEverything ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theBlobThatAteEverything :: Difficulty -> TheBlobThatAteEverything
theBlobThatAteEverything difficulty =
  sideStory TheBlobThatAteEverything "85001" "The Blob That Ate Everything" difficulty []

instance HasChaosTokenValue TheBlobThatAteEverything where
  getChaosTokenValue iid face (TheBlobThatAteEverything attrs) = case face of
    Skull -> do
      devoured <- getDevouredCount
      let divisor = if isEasyStandard attrs then 5 else 3
      pure $ ChaosTokenValue Skull (NegativeModifier $ devoured `div` divisor)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 5 7
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheBlobThatAteEverything where
  runMessage msg s@(TheBlobThatAteEverything attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup TheBlobThatAteEverything attrs do
      setup $ ul do
        li "gatherSets"
        li.validate False "epicMultiplayer"
        li.validate True "singleGroup"
        li "setAsideMiGo"
        li "setAside"
        li "subject"
        li.nested "placeLocations" do
          li "shuffleQuarantine"
          li "placeCrater"
          li "innerRing"
          li "outerRing"
          li "remaining"
          li "startAt"
        li.validate True "countermeasures"
        unscoped $ li "shuffleRemainder"

      setUsesGrid

      gather Set.TheBlobThatAteEverything
      gatherAndSetAside Set.MiGoIncursion

      placeEnemy Enemies.subject8L08 Global

      setAside
        [ Enemies.vulnerableHeart
        , Enemies.graspingOoze
        , Enemies.cubicOoze
        , Enemies.oozewraith
        , Enemies.oozewraith
        ]

      setAgendaDeck [Agendas.theAnomalySpreads, Agendas.theAnomalySwells, Agendas.theAnomalyConsumes]
      setActDeck [Acts.exposeTheAnomaly, Acts.extraterrestrialPhysiology, Acts.blackwatersBane]

      quarantine <-
        shuffle
          [ Locations.sewer
          , Locations.bridge
          , Locations.waterTower
          , Locations.church
          , Locations.oozyLakebed
          , Locations.oozyLakebed
          , Locations.slimyStreets
          , Locations.slimyStreets
          , Locations.desiccatedFarmland
          , Locations.desiccatedFarmland
          ]

      let
        quarantine' = drop 1 quarantine -- remove 1 at random
        (innerQuarantine, rest1) = splitAt 2 quarantine'
        (outerQuarantine, remainingQuarantine) = splitAt 3 rest1

      placeInGrid_ (Pos 0 0) Locations.theCrater

      innerDefs <-
        shuffle
          $ Locations.researchSiteTheBlobThatAteEverything
          : Locations.temporaryHQ
          : innerQuarantine
      innerIds <- for (zip [Pos 0 1, Pos 0 (-1), Pos 1 0, Pos (-1) 0] innerDefs) \(pos, def) -> do
        lid <- placeInGrid pos def
        pure (def, lid)

      outerDefs <- shuffle $ Locations.fungusMound : outerQuarantine
      for_ (zip [Pos 0 2, Pos 0 (-2), Pos 2 0, Pos (-2) 0] outerDefs) (uncurry placeInGrid_)

      for_
        (zip [Pos 1 1, Pos 1 (-1), Pos (-1) 1, Pos (-1) (-1)] remainingQuarantine)
        (uncurry placeInGrid_)

      for_ (lookup Locations.temporaryHQ innerIds) startAt

      playerCount <- getPlayerCount
      placeTokens ScenarioSource ScenarioTarget #resource (if playerCount >= 3 then 2 else 1)
    SetChaosTokensForScenario -> do
      setChaosTokens
        $ if isEasyStandard attrs
          then
            [ PlusOne
            , Zero
            , Zero
            , Zero
            , MinusOne
            , MinusTwo
            , MinusTwo
            , MinusThree
            , MinusFour
            , MinusFive
            , Skull
            , Skull
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
          else
            [ Zero
            , Zero
            , Zero
            , MinusOne
            , MinusOne
            , MinusTwo
            , MinusThree
            , MinusFour
            , MinusFive
            , MinusSix
            , Skull
            , Skull
            , Cultist
            , Tablet
            , ElderThing
            , AutoFail
            , ElderSign
            ]
      pure s
    ResolveChaosToken _ Cultist _ -> do
      -- If revealed during an attack against an enemy, that enemy gains
      -- retaliate for this attack.
      whenM ((== Just #fight) <$> getSkillTestAction) do
        whenJustM getSkillTestTargetedEnemy \enemy ->
          withSkillTest \sid -> skillTestModifier sid Cultist enemy (AddKeyword Keyword.Retaliate)
      pure s
    ResolveChaosToken _ Tablet _ -> do
      -- After this skill test ends, Subject 8L-08 devours each committed card.
      afterSkillTestQuiet do
        iids <- allInvestigators
        cards <- concatMapM getCommittedCards iids
        devour cards
      pure s
    ResolveChaosToken _token ElderThing iid -> do
      -- Choose up to N cards from your hand. Subject 8L-08 devours each of them.
      -- You get +1 skill value for this test for each card devoured this way.
      withSkillTest \sid -> do
        let n = if isEasyStandard attrs then 5 else 7
        hand <- field InvestigatorHand iid
        chooseUpToNM iid n "doneDevouring" do
          for_ hand \card -> cardLabeled card do
            devour [card]
            skillTestModifier sid ElderThing iid (AnySkillValue 1)
      pure s
    Defeated (EnemyTarget eid) _ _ _ -> do
      modifiers <- getModifiers eid
      unless (ScenarioModifier "noBlob" `elem` modifiers) do
        keywords <- getModifiedKeywords eid
        let blobX = [x | Keyword.ScenarioKeywordX "Blob" x <- toList keywords]
        for_ (listToMaybe blobX) \x -> do
          let extra = sum [n | ScenarioModifierValue "Blob" (maybeResult -> Just n) <- modifiers]
          subject <- selectJust $ enemyIs Enemies.subject8L08
          push $ DealDamage (EnemyTarget subject) (nonAttack Nothing attrs (x + extra))
      pure s
    EndRound ->
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "lightActive" (False :: Bool)
    -- Reality Acid records which one-time "aspects of reality" have already
    -- been devoured (per investigator or per group) in the scenario meta.
    ScenarioSpecific "blobSetMeta" (maybeResult -> Just (key, value)) ->
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey (Key.fromText key) (value :: Value)
    -- Track every card exiled during the scenario so Reality Acid can devour
    -- "all cards that have been exiled".
    Exiled _ card -> do
      exiled <- getScenarioMetaKeyDefault "exiledCards" []
      pure $ TheBlobThatAteEverything $ attrs & setMetaKey "exiledCards" (card : exiled :: [Card])
    -- Reality Acid's "concept of easiness": flip the scenario reference card to
    -- its Hard/Expert side.
    ScenarioSpecific "blobFlipToHard" _ -> do
      let harder = case attrs ^. difficultyL of
            Easy -> Hard
            Standard -> Expert
            d -> d
      pure $ TheBlobThatAteEverything $ attrs & difficultyL .~ harder
    ScenarioResolution NoResolution -> do
      push R1
      pure s
    ScenarioResolution (Resolution 1) -> scope "resolutions" do
      resolution "resolution1"
      push GameOver
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> scope "resolutions" do
      resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 3
      -- Any one investigator may add each in-play reward asset to their deck.
      rewardAssets <-
        select
          $ mapOneOf assetIs [Assets.universalSolvent, Assets.petOozeling, Assets.miGoWeapon, Assets.ltWilsonStewart]
      for_ rewardAssets addCampaignCardToDeckChoice_
      endOfScenario
      pure s
    _ -> TheBlobThatAteEverything <$> liftRunMessage msg attrs
