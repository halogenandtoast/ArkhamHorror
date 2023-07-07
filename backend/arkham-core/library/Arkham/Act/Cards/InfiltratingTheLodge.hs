module Arkham.Act.Cards.InfiltratingTheLodge (
  InfiltratingTheLodge (..),
  infiltratingTheLodge,
  infiltratingTheLodgeEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (EnemyEvaded)
import Arkham.Placement
import Arkham.Timing qualified as Timing

newtype InfiltratingTheLodge = InfiltratingTheLodge ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infiltratingTheLodge :: ActCard InfiltratingTheLodge
infiltratingTheLodge =
  act
    (1, A)
    InfiltratingTheLodge
    Cards.infiltratingTheLodge
    (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance HasAbilities InfiltratingTheLodge where
  getAbilities (InfiltratingTheLodge attrs) =
    withBaseAbilities
      attrs
      [mkAbility attrs 1 $ ReactionAbility (EnemyEvaded Timing.After You EnemyWithAnyDoom) Free]

spawnNathanWick :: LocationId -> GameT [Message]
spawnNathanWick innerSanctum = do
  nathanWick <- getSetAsideCard Enemies.nathanWickMasterOfIndoctrination
  puzzleBox <- getSetAsideCard Assets.puzzleBox
  (nathanWickId, placeNathanWick) <- createEnemyAt nathanWick innerSanctum Nothing
  puzzleBoxId <- getRandom
  pure
    [ placeNathanWick
    , CreateAssetAt puzzleBoxId puzzleBox (AttachedToEnemy nathanWickId)
    ]

instance RunMessage InfiltratingTheLodge where
  runMessage msg a@(InfiltratingTheLodge attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (evadedEnemy -> enemy) _ -> do
      push $ RemoveAllDoom (toSource attrs) (toTarget enemy)
      pure a
    AdvanceAct actId _ _ | toId attrs == actId -> do
      mInnerSanctum <- selectOne $ locationIs Locations.innerSanctum
      card <- flipCard <$> genCard (toCardDef attrs)
      let rest =
            [ PlaceNextTo ActDeckTarget [card]
            , createCardEffect Cards.infiltratingTheLodge Nothing (toSource attrs) ScenarioTarget
            , advanceActDeck attrs
            ]
      case mInnerSanctum of
        Just innerSanctum -> do
          spawnMessages <- spawnNathanWick innerSanctum
          pushAll $ spawnMessages <> rest
        Nothing -> do
          (innerSanctum, placeInnerSanctum) <- placeSetAsideLocation Locations.innerSanctum
          spawnMessages <- spawnNathanWick innerSanctum
          pushAll $ placeInnerSanctum : spawnMessages <> rest
      pure a
    _ -> InfiltratingTheLodge <$> runMessage msg attrs

newtype InfiltratingTheLodgeEffect = InfiltratingTheLodgeEffect EffectAttrs
  deriving anyclass (HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities InfiltratingTheLodgeEffect where
  getAbilities (InfiltratingTheLodgeEffect attrs) =
    [mkAbility attrs 1 $ ReactionAbility (EnemyEvaded Timing.After You EnemyWithAnyDoom) Free]

infiltratingTheLodgeEffect :: EffectArgs -> InfiltratingTheLodgeEffect
infiltratingTheLodgeEffect = cardEffect InfiltratingTheLodgeEffect Cards.infiltratingTheLodge

instance RunMessage InfiltratingTheLodgeEffect where
  runMessage msg e@(InfiltratingTheLodgeEffect attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (evadedEnemy -> enemy) _ -> do
      push $ RemoveAllDoom (toSource attrs) (toTarget enemy)
      pure e
    _ -> InfiltratingTheLodgeEffect <$> runMessage msg attrs
