module Arkham.Asset.Cards.ShiningTrapezohedron5 (
  shiningTrapezohedron5,
  ShiningTrapezohedron5 (..),
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

newtype ShiningTrapezohedron5 = ShiningTrapezohedron5 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shiningTrapezohedron5 :: AssetCard ShiningTrapezohedron5
shiningTrapezohedron5 = asset ShiningTrapezohedron5 Cards.shiningTrapezohedron5

instance HasAbilities ShiningTrapezohedron5 where
  getAbilities (ShiningTrapezohedron5 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility (WouldPayCardCost #when You #spell) (exhaust a)
    ]

instance HasModifiersFor ShiningTrapezohedron5 where
  getModifiersFor (InvestigatorTarget iid) (ShiningTrapezohedron5 attrs) | attrs `controlledBy` iid = do
    pure $ toModifiers attrs [CanModify $ AlternateResourceCost #spell Free | not attrs.exhausted]
  getModifiersFor _ _ = pure []

getWindowCard :: [Window] -> (ActiveCostId, BatchId, Card)
getWindowCard [] = error "No window card"
getWindowCard ((windowType -> Window.WouldPayCardCost _ acId batchId card) : _) = (acId, batchId, card)
getWindowCard (_ : rest) = getWindowCard rest

instance RunMessage ShiningTrapezohedron5 where
  runMessage msg a@(ShiningTrapezohedron5 attrs) = case msg of
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
    _ -> ShiningTrapezohedron5 <$> runMessage msg attrs
