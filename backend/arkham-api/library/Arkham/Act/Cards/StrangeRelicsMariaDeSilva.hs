module Arkham.Act.Cards.StrangeRelicsMariaDeSilva (
  StrangeRelicsMariaDeSilva (..),
  strangeRelicsMariaDeSilva,
) where

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
strangeRelicsMariaDeSilva =
  act (2, E) StrangeRelicsMariaDeSilva Cards.strangeRelicsMariaDeSilva Nothing

instance HasAbilities StrangeRelicsMariaDeSilva where
  getAbilities (StrangeRelicsMariaDeSilva a) =
    [ restrictedAbility
      a
      1
      (exists $ assetIs Assets.mariaDeSilva <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide E a
    ]

instance RunMessage StrangeRelicsMariaDeSilva where
  runMessage msg a@(StrangeRelicsMariaDeSilva attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      maria <- selectJust $ assetIs Assets.mariaDeSilva
      mariasLocation <- selectJust $ LocationWithAsset $ AssetWithId maria
      enemyMaria <- lookupCard Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn <$> field AssetCardId maria
      createEnemyAt_ enemyMaria mariasLocation
      pushAll
        [ Flipped (toSource maria) enemyMaria
        , NextAdvanceActStep aid 1
        , AdvanceToAct (actDeckId attrs) Acts.theBrotherhoodIsRevealed E (toSource attrs)
        ]
      pure a
    NextAdvanceActStep (isSide F attrs -> True) 1 -> do
      maria <- selectJust (enemyIs Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn)
      card <- field EnemyCard maria
      rememberIchtacasPrey maria card
      pure a
    _ -> StrangeRelicsMariaDeSilva <$> liftRunMessage msg attrs
