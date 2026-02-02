module Arkham.Agenda.Cards.DeepeningDark (deepeningDark) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (RevealLocation)
import Arkham.Direction
import Arkham.Helpers.Enemy (spawnAt)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window (getRevealedLocation)
import Arkham.I18n
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn
import Arkham.Token
import Arkham.Trait (Trait (Dark))

newtype DeepeningDark = DeepeningDark AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepeningDark :: AgendaCard DeepeningDark
deepeningDark = agenda (1, A) DeepeningDark Cards.deepeningDark (Static 3)

instance HasModifiersFor DeepeningDark where
  getModifiersFor (DeepeningDark a) =
    modifySelect a Anyone [CannotCancelHorrorFrom (a.ability 2)]

instance HasAbilities DeepeningDark where
  getAbilities (DeepeningDark x) =
    [ restricted x 1 (ScenarioDeckWithCard WoodsDeck)
        $ forced
        $ RevealLocation #after Anyone Anywhere
    , restricted x 2 (exists $ InvestigatorAt $ LocationWithTrait Dark)
        $ forced
        $ PhaseEnds #when #investigation
    ]

instance RunMessage DeepeningDark where
  runMessage msg a@(DeepeningDark attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      placeTokens attrs ScenarioTarget DarknessLevel 1
      doStep 1 msg
      pure a
    DoStep 1 msg'@(AdvanceAgenda (isSide B attrs -> True)) -> do
      darknessLevel <- scenarioFieldMap ScenarioTokens (countTokens DarknessLevel)
      isStandalone <- getIsStandalone
      if (darknessLevel >= 6) && not isStandalone
        then eachInvestigator \iid -> do
          sufferPhysicalTrauma iid 1
          investigatorDefeated attrs iid
        else do
          selectEach (EnemyAt (LocationWithTrait Dark) <> UnengagedEnemy) (`place` PursuitZone)
          unrevealed <- select $ UnrevealedLocation <> LocationWithTrait Dark
          unless (null unrevealed) do
            shuffleCardsIntoTopOfDeck WoodsDeck 2 unrevealed
          selectEach (RevealedLocation <> EmptyLocation) \loc -> do
            push $ RemoveAllTokens (toSource attrs) (toTarget loc)
            push $ UnrevealLocation loc
          doStep 2 msg'
          doStep 3 msg'
          revertAgenda attrs
      pure a
    DoStep 2 msg'@(AdvanceAgenda (isSide B attrs -> True)) -> do
      selectEach (LocationWithInvestigator Anyone) (`forTarget_` msg')
      pure a
    DoStep 3 (AdvanceAgenda (isSide B attrs -> True)) -> do
      mowner <- selectOne (HasMatchingAsset $ AssetWithTitle "Vale Lantern")
      for_ mowner \owner -> do
        sid <- getRandom
        darknessLevel <- scenarioFieldMap ScenarioTokens (countTokens DarknessLevel)
        beginSkillTest sid owner attrs owner #willpower (Fixed darknessLevel)
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      enemies <- select $ OutOfPlayEnemy PursuitZone AnyEnemy
      chooseTargetM iid enemies \enemy -> spawnAt enemy (Just iid) (SpawnEngagedWith $ InvestigatorWithId iid)
      pure a
    ForTarget (LocationTarget loc) (AdvanceAgenda (isSide B attrs -> True)) -> do
      woodsDeck <- getScenarioDeck WoodsDeck
      grid <- getGrid
      let
        locationPositions = case findInGrid loc grid of
          Nothing -> []
          Just pos -> emptyPositionsInDirections grid pos [GridUp ..]
      for_ (zip locationPositions woodsDeck) (uncurry placeLocationInGrid)
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (getRevealedLocation -> lid) _ -> do
      woodsDeck <- getScenarioDeck WoodsDeck
      grid <- getGrid
      let
        locationPositions = case findInGrid lid grid of
          Nothing -> []
          Just pos -> emptyPositionsInDirections grid pos [GridUp ..]
      for_ (zip locationPositions woodsDeck) (uncurry placeLocationInGrid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (InvestigatorAt $ LocationWithTrait Dark) (`forInvestigator` msg)
      pure a
    ForInvestigator iid (UseThisAbility _ (isSource attrs -> True) 2) -> do
      enemies <- pursuitEnemiesWithHighestEvade
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 1 $ labeled' "takeHorror" (assignHorror iid (attrs.ability 1) 1)
        labeledValidate' (notNull enemies) "pursuitEnemy" do
          chooseOrRunOneM iid do
            targets enemies $ push . InvestigatorDrawEnemy iid
      pure a
    _ -> DeepeningDark <$> liftRunMessage msg attrs
