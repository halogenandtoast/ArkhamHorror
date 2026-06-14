module Arkham.Act.Cards.DeadlySkies (deadlySkies) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype DeadlySkies = DeadlySkies ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlySkies :: ActCard DeadlySkies
deadlySkies = act (2, A) DeadlySkies Cards.deadlySkies Nothing

instance HasAbilities DeadlySkies where
  getAbilities (DeadlySkies a) =
    extend
      a
      [ restricted a 1 (Here <> CanManipulateDeck) $ actionAbilityWithCost ClueCostX
      , restricted
          a
          2
          (EachUndefeatedInvestigator $ at_ $ locationIs Locations.floatingSpire)
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage DeadlySkies where
  runMessage msg a@(DeadlySkies attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (totalCluePayment -> _x) -> do
      -- Spend X clues: reveal X cards from the BOTTOM of the Summit deck; you may
      -- put 1 revealed location into play in an adjacent open sky and move to it
      -- (placing the open sky card + the other revealed cards back on top of the
      -- Summit deck).
      --
      -- TODO: The Summit deck, the "open sky" placeholder cards, and "sliding"
      -- locations have no engine support yet. Once that infra exists, reveal _x
      -- cards from the bottom of the Summit deck, let the investigator optionally
      -- place one revealed location into an adjacent open sky and moveTo it, then
      -- return the open sky card and the remaining revealed cards to the top of
      -- the Summit deck.
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Side B: remove Central Spire from play and rebuild the Summit around the
      -- Floating Spire.
      --
      -- TODO: Removing the Central Spire and rebuilding the Summit grid around the
      -- Floating Spire depends on the unimplemented Summit deck / sliding-location
      -- infrastructure. Once available, remove Locations.centralSpire from play and
      -- reconfigure the grid here.
      advanceActDeck attrs
      pure a
    _ -> DeadlySkies <$> liftRunMessage msg attrs
