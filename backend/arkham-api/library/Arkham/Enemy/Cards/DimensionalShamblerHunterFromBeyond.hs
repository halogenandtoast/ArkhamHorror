module Arkham.Enemy.Cards.DimensionalShamblerHunterFromBeyond (dimensionalShamblerHunterFromBeyond) where

import Arkham.Ability
import Arkham.Action
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTowards)
import Arkham.Window qualified as Window

newtype DimensionalShamblerHunterFromBeyond = DimensionalShamblerHunterFromBeyond EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalShamblerHunterFromBeyond :: EnemyCard DimensionalShamblerHunterFromBeyond
dimensionalShamblerHunterFromBeyond =
  enemy
    DimensionalShamblerHunterFromBeyond
    Cards.dimensionalShamblerHunterFromBeyond
    (6, Static 5, 2)
    (2, 2)

instance HasAbilities DimensionalShamblerHunterFromBeyond where
  getAbilities (DimensionalShamblerHunterFromBeyond a) =
    extend1 a
      $ restricted a 1 (thisExists a $ ReadyEnemy <> UnengagedEnemy)
      $ forced
      $ ScenarioEvent #when (Just You) "triggeredShambler"

instance RunMessage DimensionalShamblerHunterFromBeyond where
  runMessage msg e@(DimensionalShamblerHunterFromBeyond attrs) = runQueueT $ case msg of
    TakenActions iid actions -> do
      let mstored = maybeResult @(InvestigatorId, [Action]) attrs.meta
      for_ mstored \(iid', actions') ->
        when (iid == iid' && any (`elem` actions') actions) do
          checkWhen $ Window.ScenarioEvent "triggeredShambler" (Just iid) Null
      pure $ DimensionalShamblerHunterFromBeyond $ attrs & setMeta (iid, actions)
    Begin _ -> do
      pure $ DimensionalShamblerHunterFromBeyond $ attrs & setMeta Null
    EndPhase -> do
      pure $ DimensionalShamblerHunterFromBeyond $ attrs & setMeta Null
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid $ moveTowards (attrs.ability 1) attrs
      pure e
    _ -> DimensionalShamblerHunterFromBeyond <$> liftRunMessage msg attrs
