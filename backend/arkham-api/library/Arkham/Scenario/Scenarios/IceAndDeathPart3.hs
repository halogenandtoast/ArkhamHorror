module Arkham.Scenario.Scenarios.IceAndDeathPart3 (iceAndDeathPart3) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Capability
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosBag (hasRemainingFrostTokens)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Helpers.Text
import Arkham.Helpers.Xp (toBonus)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.IceAndDeath.Helpers
import Arkham.Trait (Trait (Eidolon))

newtype IceAndDeathPart3 = IceAndDeathPart3 ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceAndDeathPart3 :: Difficulty -> IceAndDeathPart3
iceAndDeathPart3 difficulty =
  scenarioWith
    IceAndDeathPart3
    "08501c"
    "Ice and Death"
    difficulty
    iceAndDeathLayout
    (referenceL .~ "08501")

instance HasModifiersFor IceAndDeathPart3 where
  getModifiersFor (IceAndDeathPart3 a) = do
    getCamp >>= traverse_ \camp -> modifySelect a (locationIs camp) [ScenarioModifier "camp"]

instance HasChaosTokenValue IceAndDeathPart3 where
  getChaosTokenValue iid tokenFace (IceAndDeathPart3 attrs) = case tokenFace of
    Skull -> do
      n <-
        fromMaybe 0 <$> runMaybeT do
          lid <- MaybeT $ getMaybeLocation iid
          MaybeT $ shelterValue lid
      pure $ toChaosTokenValue attrs Skull ((n + 1) `div` 2) n
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage IceAndDeathPart3 where
  runMessage msg s@(IceAndDeathPart3 attrs) = runQueueT $ scenarioI18n 3 $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      whenM hasRemainingFrostTokens $ addChaosToken #frost
      sv <- fromMaybe 0 <$> getCurrentShelterValue
      story $ withVars ["shelterValue" .= sv] $ i18nWithTitle "investigatorSetup"
      eachInvestigator (`forInvestigator` PreScenarioSetup)
      pure s
    ForInvestigator iid PreScenarioSetup -> do
      getCurrentShelterValue >>= traverse_ \sv -> do
        setupModifier ScenarioSource iid (BaseStartingResources sv)

      partners <- filterM (selectNone . assetIs) =<< getRemainingPartners

      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          labeled "Do not take a partner" nothing
          for_ partners \partner -> do
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
    Setup -> runScenarioSetup IceAndDeathPart3 attrs do
      gather Set.IceAndDeath
      gather Set.SeepingNightmares
      gather Set.CreaturesInTheIce
      gather Set.DeadlyWeather
      gather Set.HazardsOfAntarctica
      gather Set.SilenceAndMystery
      gather Set.Tekelili
      gather Set.AncientEvils

      revealed <- getRecordSet LocationsRevealed
      n <- perPlayer 1
      (seepingNightmares, removes) <- splitAt n <$> amongGathered (cardIs Enemies.seepingNightmare)
      removeCards $ seepingNightmares <> removes

      camp <- fromJustNote "Missing camp" <$> getCamp

      let
        placeSite def = do
          location <- place def
          when (recorded (toCardCode def) `elem` revealed) $ reveal location
          when (def == camp) do
            push $ RemoveAllClues ScenarioSource (toTarget location)
            startAt location
          pure location

      crashSite <- placeSite Locations.crashSite
      _frozenShores <- placeSite Locations.frozenShores
      _treacherousPath <- placeSite Locations.treacherousPath
      _precariousIceSheet <- placeSite Locations.precariousIceSheet
      broadSnowdrifts <- placeSite Locations.broadSnowdrifts
      icyWastes <- placeSite Locations.icyWastes
      rockyCrags <- placeSite Locations.rockyCrags
      snowGraves <- placeSite Locations.snowGraves
      icebreakerLanding <- placeSite Locations.icebreakerLanding
      frigidCave <- placeSite Locations.frigidCave
      barrierCamp <- placeSite Locations.barrierCamp
      remnantsOfLakesCamp <- placeSite Locations.remnantsOfLakesCamp
      crystallineCavern <- placeSite Locations.crystallineCavern

      let
        locations =
          if
            | camp
                `elem` [ Locations.crashSite
                       , Locations.treacherousPath
                       , Locations.precariousIceSheet
                       , Locations.frozenShores
                       ] ->
                [icyWastes, broadSnowdrifts, rockyCrags, icebreakerLanding]
            | camp `elem` [Locations.icyWastes, Locations.icebreakerLanding] ->
                [snowGraves, frigidCave, remnantsOfLakesCamp, barrierCamp]
            | camp `elem` [Locations.broadSnowdrifts, Locations.snowGraves] ->
                [icebreakerLanding, frigidCave, remnantsOfLakesCamp, crystallineCavern]
            | camp `elem` [Locations.frigidCave, Locations.rockyCrags] ->
                [icebreakerLanding, frigidCave, remnantsOfLakesCamp, crystallineCavern]
            | camp == Locations.crystallineCavern ->
                [remnantsOfLakesCamp, barrierCamp, icebreakerLanding, crashSite]
            | camp == Locations.barrierCamp -> [remnantsOfLakesCamp, crystallineCavern, snowGraves, crashSite]
            | otherwise -> [barrierCamp, crystallineCavern, frigidCave, crashSite] -- RemnantsOfLakesCamp
      seepingNightmareEnemies <- for (zip seepingNightmares locations) (uncurry createEnemyAt)

      eidolons <- shuffle =<< amongGathered (CardWithTrait Eidolon)

      case seepingNightmareEnemies of
        [a] -> placeUnderneath a eidolons
        [a, b] -> case splitAt 4 eidolons of
          (x, y) -> do
            placeUnderneath a x
            placeUnderneath b y
        [a, b, c] -> case splitAt 6 eidolons of
          (x', _extra) -> case splitAt 2 x' of
            (x, y') -> case splitAt 2 y' of
              (y, z) -> do
                placeUnderneath a x
                placeUnderneath b y
                placeUnderneath c z
        [a, b, c, d] ->
          -- split into 4 groups of 2
          case splitAt 4 eidolons of
            (x, y) -> do
              case splitAt 2 x of
                (x', y') -> do
                  placeUnderneath a x'
                  placeUnderneath b y'
              case splitAt 2 y of
                (x', y') -> do
                  placeUnderneath c x'
                  placeUnderneath d y'
        _ -> error "Invalid number of Seeping Nightmares"

      setAgendaDeck [Agendas.manifestationsOfEvil, Agendas.icyDepths]
      setActDeck [Acts.underAttack]

      addTekeliliDeck
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist -> do
          let x = if isEasyStandard attrs then 1 else n
          tekelili <- take x <$> getScenarioDeck TekeliliDeck
          canModifyDeck <- can.manipulate.deck iid
          if null tekelili || not canModifyDeck
            then assignHorror iid Cultist x
            else do
              addTekelili iid tekelili
              when (length tekelili < x) $ assignHorror iid Cultist (x - length tekelili)
        Tablet -> push $ DiscardTopOfDeck iid n (toSource Tablet) (Just $ toTarget attrs)
        _ -> pure ()
      pure s
    DiscardedTopOfDeck iid cards (isSource Tablet -> True) (isTarget attrs -> True) -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) cards
      when (notNull weaknesses) $ addToHand iid weaknesses
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          insight <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.seepingNightmare
          xp <-
            if insight
              then allGainXpWithBonus' attrs $ toBonus "insight" 2
              else allGainXp' attrs
          story
            $ withVars
              [ "bonusClass" .= String (if insight then "valid" else "invalid")
              , "xp" .= xp
              ]
            $ i18nWithTitle "noResolution"
          record TheTeamBarelyEscapedTheIceShelf
          endOfScenario
        Resolution 1 -> do
          xp <- allGainXpWithBonus' attrs $ toBonus "insight" 5
          story $ withVars ["xp" .= xp] $ i18nWithTitle "resolution1"
          record TheTeamDefeatedTheHuntingCreatures
          endOfScenario
        Resolution 2 -> do
          insight <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.seepingNightmare
          xp <-
            if insight
              then allGainXpWithBonus' attrs $ toBonus "insight" 2
              else allGainXp' attrs
          story
            $ withVars
              [ "bonusClass" .= String (if insight then "valid" else "invalid")
              , "xp" .= xp
              ]
            $ i18nWithTitle "resolution2"
          record TheTeamFledToTheMountains
          endOfScenario
        _ -> error "Unknown resolution"
      pure s
    _ -> IceAndDeathPart3 <$> liftRunMessage msg attrs
