module Arkham.Asset.Assets.ShiningTrapezohedron4 (shiningTrapezohedron4, ShiningTrapezohedron4 (..)) where

import Arkham.Ability
import Arkham.ActiveCost.Base
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Name
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ShiningTrapezohedron4 = ShiningTrapezohedron4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shiningTrapezohedron4 :: AssetCard ShiningTrapezohedron4
shiningTrapezohedron4 = asset ShiningTrapezohedron4 Cards.shiningTrapezohedron4

instance HasAbilities ShiningTrapezohedron4 where
  getAbilities (ShiningTrapezohedron4 a) =
    [ skillTestAbility
        $ restricted a 1 ControlsThis
        $ ReactionAbility (WouldPayCardCost #when You #spell) (exhaust a)
    ]

instance HasModifiersFor ShiningTrapezohedron4 where
  getModifiersFor (ShiningTrapezohedron4 a) = case a.controller of
    Just iid | a.ready -> modified_ a iid [CanModify $ AlternateResourceCost #spell Free]
    _ -> pure mempty

getWindowCard :: [Window] -> (ActiveCostId, BatchId, Card)
getWindowCard [] = error "No window card"
getWindowCard ((windowType -> Window.WouldPayCardCost _ acId batchId card) : _) = (acId, batchId, card)
getWindowCard (_ : rest) = getWindowCard rest

instance RunMessage ShiningTrapezohedron4 where
  runMessage msg a@(ShiningTrapezohedron4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getWindowCard -> (acId, _batchId, card)) _ -> do
      sid <- getRandom
      beginSkillTest
        sid
        iid
        (attrs.ability 1)
        (ActiveCostTarget acId)
        #willpower
        (CardCostCalculation iid $ toCardId card)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
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
            ForCard _ c -> do
              push $ CancelCost acId
              roundModifier (attrs.ability 1) iid (CannotPlay $ CardWithTitle $ toTitle c)
            _ -> error "wrong cost target"
        _ -> error "invalid target"

      pure a
    _ -> ShiningTrapezohedron4 <$> liftRunMessage msg attrs
