module Arkham.Homebrew.CircusExMortis.Scenarios.HarmsWay (harmsWay) where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Enemy.Types (Field (EnemyAsSelfLocation))
import Arkham.Helpers (unDeck)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.History (getHistoryField)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.History
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Id (AgendaId (..))
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Location.Grid (Pos (..), gridLabel)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Story
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Bystander))

newtype HarmsWay = HarmsWay ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harmsWay :: Difficulty -> HarmsWay
harmsWay difficulty = scenario HarmsWay ":circus-ex-mortis:040" "Harm's Way" difficulty []

crowdedRows :: [CardDef]
crowdedRows =
  [ Locations.crowdedRow_048
  , Locations.crowdedRow_049
  , Locations.crowdedRow_050
  , Locations.crowdedRow_051
  ]

secludedTents :: [CardDef]
secludedTents =
  [ Locations.secludedTent_052
  , Locations.secludedTent_053
  , Locations.secludedTent_054
  , Locations.secludedTent_055
  ]

toweringDarkYoungs :: [CardDef]
toweringDarkYoungs =
  [ Enemies.toweringDarkYoung_065
  , Enemies.toweringDarkYoung_066
  , Enemies.toweringDarkYoung_067
  , Enemies.toweringDarkYoung_068
  , Enemies.toweringDarkYoung_069
  ]

{- | The six Kidnapped Citizen cards, by their front codes. Each is a single
story entity whose own module starts it flipped to the Kidnapped Citizen face,
so setup places them like any other story and needs no orientation handling.
-}
kidnappedCitizens :: [CardDef]
kidnappedCitizens =
  [ Stories.hiddenInPlainSight
  , Stories.underLockAndKey
  , Stories.cautiousJailers
  , Stories.deepInTheDark
  , Stories.clappedInIrons
  , Stories.hypnoticState
  ]

instance HasChaosTokenValue HarmsWay where
  getChaosTokenValue iid tokenFace (HarmsWay attrs) = case tokenFace of
    Skull -> do
      bystanders <- selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTrait Bystander
      let extra = if isHardExpert attrs then 1 else 0
      pure $ ChaosTokenValue Skull $ NegativeModifier $ extra + ((bystanders + 1) `div` 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage HarmsWay where
  runMessage msg s@(HarmsWay attrs) = runQueueT $ scenarioI18n "harmsWay" $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM (setTitle "title" >> p "body") do
        labeled "faster" $ addChaosToken Cultist
        labeled "caution" $ addChaosToken Tablet
      pure s
    Setup -> runScenarioSetup HarmsWay attrs do
      gather Set.HarmsWay
      gather Set.CultOfShubNiggurath
      gather Set.LunaticNight
      gather Set.NewMoonEntertainers
      gather Set.PanickedMasses

      (removedRows, keptRows) <- splitAt 2 <$> shuffle crowdedRows
      (removedTents, keptTents) <- splitAt 2 <$> shuffle secludedTents
      removeEvery (removedRows <> removedTents)

      ringmastersTrailer <- placeInGrid (Pos 0 0) Locations.ringmastersTrailer
      camps <-
        for (zip [Pos 0 1, Pos 0 (-1), Pos (-1) 0, Pos 1 0] (keptRows <> keptTents)) (uncurry placeInGrid)
      startAt ringmastersTrailer

      (removedYoung, keptYoung) <- splitAt 1 <$> shuffle toweringDarkYoungs
      removeEvery removedYoung
      -- They are at no location, so InPosition and not Global: Global means "on
      -- the same location as everyone", which would make all four fightable
      -- from anywhere. The four corners are the otherwise-empty diagonals
      -- around Ringmaster's Trailer at (0, 0).
      let corners = [Pos (-1) 1, Pos 1 1, Pos (-1) (-1), Pos 1 (-1)]
      for_ (zip keptYoung corners) \(def, pos) -> do
        eid <- placeEnemyCapture def (InPosition pos)
        push $ UpdateEnemy eid $ Update EnemyAsSelfLocation (Just $ gridLabel pos)

      initFuryBag
      placeStory Stories.theDarkYoungStir

      (removedCitizens, keptCitizens) <- splitAt 2 <$> shuffle kidnappedCitizens
      removeEvery removedCitizens
      for_ (zip camps keptCitizens) \(lid, def) -> do
        card <- genCard def
        removeEvery [def]
        push $ StoryMessage $ PlaceStory card (AtLocation lid)

      -- "Place 2 doom on agenda 1a. This doom ignores the forced effect."
      whenM (getHasRecord TheInvestigatorsBypassedTheIllusions) do
        scenarioSetupModifier
          attrs.id
          attrs
          (AgendaId $ toCardCode Agendas.theCircusSleeps)
          (EntersPlayWithDoom 2)

      eyeOnYou <- getHasRecord TheRingmasterHasHisEyeOnYou
      let (act1, unusedAct1) =
            if eyeOnYou then (Acts.escapeActVI, Acts.escapeActVII) else (Acts.escapeActVII, Acts.escapeActVI)
      removeEvery [unusedAct1]

      setAside [Locations.campOutskirtsGuardedClosely, Locations.campOutskirtsQuietForNow]

      -- "The investigators with Amalthea Weaver and De Cultus Bestiae in their
      -- decks may begin the game with those cards in their opening hands as
      -- additional cards."
      owners <- catMaybes <$> sequence [getAmaltheaWeaverOwner, getDeCultusBestiaeOwner]
      for_ owners \(iid, def) -> do
        deck <- field InvestigatorDeck iid
        for_ (find ((== def) . toCardDef) (unDeck deck)) \card -> do
          push $ ObtainCard (toCardId card)
          setupModifier ScenarioSource iid (AdditionalStartingCards [toCard card])

      setAgendaDeck [Agendas.theCircusSleeps, Agendas.treadingOnEggshells, Agendas.sleepWhenYoureDead]
      setActDeck [act1, Acts.overdueDeparture]
    ResolveChaosToken _ Cultist iid -> do
      moved <- getHistoryField RoundHistory iid HistoryMoved
      when (moved == 0) $ doStep 1 msg
      pure s
    ResolveChaosToken _ Tablet iid -> do
      moved <- getHistoryField RoundHistory iid HistoryMoved
      when (moved > 0) $ doStep 1 msg
      pure s
    DoStep 1 (ResolveChaosToken _ _ iid) -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "discardCards" $ chooseAndDiscardCard iid ScenarioSource
        countVar 1 $ labeled "loseResources" $ loseResources iid ScenarioSource 1
      pure s
    FailedSkillTestWithToken _ ElderThing -> do
      revealFuryToken ScenarioSource
      pure s
    ScenarioResolution r -> scope "resolutions" do
      citizens <- select $ VictoryDisplayCardMatch $ basic $ CardWithTitle "Kidnapped Citizen"
      case r of
        _ | r `elem` [NoResolution, Resolution 1] -> do
          resolution "resolution1"
          -- "Remove 2 copies of Kidnapped Citizen from the victory display, if
          -- possible", so they neither count for X nor pay out their Victory 1.
          for_ (take 2 $ mapMaybe (preview _EncounterCard) citizens) (push . AddToEncounterDiscard)
          recordCount GroupsOfCitizensWereSavedFromTheCircus $ max 0 (length citizens - 2)
          push R3
        Resolution 2 -> do
          resolution "resolution2"
          recordCount GroupsOfCitizensWereSavedFromTheCircus (length citizens)
          push R3
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> HarmsWay <$> liftRunMessage msg attrs
