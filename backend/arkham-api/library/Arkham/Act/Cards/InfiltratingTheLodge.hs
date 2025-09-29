module Arkham.Act.Cards.InfiltratingTheLodge (infiltratingTheLodge, infiltratingTheLodgeEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Window (evadedEnemy)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype InfiltratingTheLodge = InfiltratingTheLodge ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infiltratingTheLodge :: ActCard InfiltratingTheLodge
infiltratingTheLodge =
  act (1, A) InfiltratingTheLodge Cards.infiltratingTheLodge (groupClueCost (PerPlayer 2))

instance HasAbilities InfiltratingTheLodge where
  getAbilities (InfiltratingTheLodge attrs) =
    extend attrs [mkAbility attrs 1 $ freeReaction (EnemyEvaded #after You EnemyWithAnyDoom)]

instance RunMessage InfiltratingTheLodge where
  runMessage msg a@(InfiltratingTheLodge attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (evadedEnemy -> enemy) _ -> do
      removeAllDoom attrs enemy
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      innerSanctum <-
        selectOne (locationIs Locations.innerSanctum) >>= \case
          Just loc -> pure loc
          Nothing -> placeSetAsideLocation Locations.innerSanctum
      nathanWickCard <- fetchCard Enemies.nathanWickMasterOfIndoctrination
      puzzleBox <- fetchCard Assets.puzzleBox
      nathanWick <- createEnemyAt nathanWickCard innerSanctum
      createAssetAt_ puzzleBox (AttachedToEnemy nathanWick)

      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      createCardEffect Cards.infiltratingTheLodge Nothing (toSource attrs) ScenarioTarget
      advanceActDeck attrs
      pure a
    _ -> InfiltratingTheLodge <$> liftRunMessage msg attrs

newtype InfiltratingTheLodgeEffect = InfiltratingTheLodgeEffect EffectAttrs
  deriving anyclass (HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities InfiltratingTheLodgeEffect where
  getAbilities (InfiltratingTheLodgeEffect attrs) =
    [ mkAbility (proxied (ActMatcherSource AnyAct) attrs) 1
        $ freeReaction (EnemyEvaded #after You EnemyWithAnyDoom)
    ]

infiltratingTheLodgeEffect :: EffectArgs -> InfiltratingTheLodgeEffect
infiltratingTheLodgeEffect = cardEffect InfiltratingTheLodgeEffect Cards.infiltratingTheLodge

instance RunMessage InfiltratingTheLodgeEffect where
  runMessage msg e@(InfiltratingTheLodgeEffect attrs) = runQueueT case msg of
    UseCardAbility _ (isProxySource attrs -> True) 1 (evadedEnemy -> enemy) _ -> do
      removeAllDoom attrs enemy
      pure e
    _ -> InfiltratingTheLodgeEffect <$> liftRunMessage msg attrs
