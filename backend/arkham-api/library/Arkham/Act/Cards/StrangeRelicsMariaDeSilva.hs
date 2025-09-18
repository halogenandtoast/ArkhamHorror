module Arkham.Act.Cards.StrangeRelicsMariaDeSilva (strangeRelicsMariaDeSilva) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (AssetCard)
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
      removeAsset maria
      enemyMaria <- createEnemyAt Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn mariasLocation
      rememberIchtacasPrey enemyMaria Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn
      advanceToAct attrs Acts.theBrotherhoodIsRevealed E
      pure a
    _ -> StrangeRelicsMariaDeSilva <$> liftRunMessage msg attrs
