module Arkham.Act.Cards.StrangeRelicsMariaDeSilva (strangeRelicsMariaDeSilva) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype StrangeRelicsMariaDeSilva = StrangeRelicsMariaDeSilva ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeRelicsMariaDeSilva :: ActCard StrangeRelicsMariaDeSilva
strangeRelicsMariaDeSilva = act (2, E) StrangeRelicsMariaDeSilva Cards.strangeRelicsMariaDeSilva Nothing

instance HasAbilities StrangeRelicsMariaDeSilva where
  getAbilities = actAbilities1' E \a ->
    restricted a 1 (exists $ assetIs Assets.mariaDeSilva <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage StrangeRelicsMariaDeSilva where
  runMessage msg a@(StrangeRelicsMariaDeSilva attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide F attrs -> True) _ _ -> do
      maria <- selectJust $ assetIs Assets.mariaDeSilva
      mariasLocation <- selectJust $ locationWithAsset maria
      enemyMaria <- lookupCard Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn <$> field AssetCardId maria
      createEnemyAt_ enemyMaria mariasLocation
      push $ Flipped (toSource maria) enemyMaria
      doStep 1 msg
      advanceToAct attrs Acts.theBrotherhoodIsRevealed E
      pure a
    DoStep 1 (AdvanceAct (isSide F attrs -> True) _ _) -> do
      maria <- selectJust (enemyIs Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn)
      rememberIchtacasPrey maria =<< field EnemyCard maria
      pure a
    _ -> StrangeRelicsMariaDeSilva <$> liftRunMessage msg attrs
