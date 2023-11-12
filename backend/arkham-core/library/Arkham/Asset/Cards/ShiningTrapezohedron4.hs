module Arkham.Asset.Cards.ShiningTrapezohedron4 (
  shiningTrapezohedron4,
  ShiningTrapezohedron4 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ActiveCost.Base
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ShiningTrapezohedron4 = ShiningTrapezohedron4 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shiningTrapezohedron4 :: AssetCard ShiningTrapezohedron4
shiningTrapezohedron4 = asset ShiningTrapezohedron4 Cards.shiningTrapezohedron4

instance HasAbilities ShiningTrapezohedron4 where
  getAbilities (ShiningTrapezohedron4 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility (WouldPayCardCost #when You #spell) (exhaust a)
    ]

instance HasModifiersFor ShiningTrapezohedron4 where
  getModifiersFor (InvestigatorTarget iid) (ShiningTrapezohedron4 attrs) | attrs `controlledBy` iid = do
    pure $ toModifiers attrs [CanModify $ AlternateResourceCost #spell Free | not attrs.exhausted]
  getModifiersFor _ _ = pure []

getWindowCard :: [Window] -> (ActiveCostId, BatchId, Card)
getWindowCard [] = error "No window card"
getWindowCard ((windowType -> Window.WouldPayCardCost _ acId batchId card) : _) = (acId, batchId, card)
getWindowCard (_ : rest) = getWindowCard rest

instance RunMessage ShiningTrapezohedron4 where
  runMessage msg a@(ShiningTrapezohedron4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getWindowCard -> (acId, _batchId, card)) _ -> do
      resources <- getModifiedCardCost iid card
      push $ beginSkillTest iid (toAbilitySource attrs 1) (ActiveCostTarget acId) #willpower resources
      pure a
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      costs <- getActiveCosts
      mTarget <- getSkillTestTarget

      let
        replaceResourceCost = \case
          ResourceCost _ -> Free
          Costs cs -> Costs $ map replaceResourceCost cs
          other -> other

      case mTarget of
        Just (ActiveCostTarget acId) -> case find ((== acId) . activeCostId) costs of
          Nothing -> error "invalid target"
          Just cost -> push $ SetCost acId (replaceResourceCost $ activeCostCosts cost)
        _ -> error "invalid target"
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      costs <- getActiveCosts
      mTarget <- getSkillTestTarget

      case mTarget of
        Just (ActiveCostTarget acId) -> case find ((== acId) . activeCostId) costs of
          Nothing -> error "invalid target"
          Just cost -> case activeCostTarget cost of
            ForCard _ c ->
              pushAll
                [ CancelCost acId
                , roundModifier (toAbilitySource attrs 1) iid (CannotPlay $ CardWithTitle $ toTitle c)
                ]
            _ -> error "wrong cost target"
        _ -> error "invalid target"

      pure a
    _ -> ShiningTrapezohedron4 <$> runMessage msg attrs
