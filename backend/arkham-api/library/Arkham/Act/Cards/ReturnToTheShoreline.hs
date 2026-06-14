module Arkham.Act.Cards.ReturnToTheShoreline (returnToTheShoreline) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Matcher

newtype ReturnToTheShoreline = ReturnToTheShoreline ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToTheShoreline :: ActCard ReturnToTheShoreline
returnToTheShoreline = act (3, A) ReturnToTheShoreline Cards.returnToTheShoreline Nothing

instance HasAbilities ReturnToTheShoreline where
  getAbilities = actAbilities \a ->
    [ -- [action] Spend X clues: Reveal X cards from the bottom of the Summit deck...
      mkAbility a 1 actionAbility
    , -- Objective: if each surviving investigator has resigned, advance.
      restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage ReturnToTheShoreline where
  runMessage msg a@(ReturnToTheShoreline attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Spend X clues": let the investigator choose how many clues to commit to
      -- the reveal. X is bounded by the clues they can spend.
      spendable <- getSpendableClueCount iid
      chooseAmount iid "Spend Clues" "Clues" 0 spendable attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Clues" -> x) (isTarget attrs -> True) | x > 0 -> do
      spendClues iid x
      -- TODO: The "Summit deck" (the deck of open-sky / location cards drawn for the
      -- floating sky-city) has no engine support yet. Once it exists, the rest of the
      -- ability resolves here:
      --   * Reveal X cards from the BOTTOM of the Summit deck.
      --   * You may put 1 revealed location into play in an adjacent open sky and move
      --     to it.
      --   * Place the open-sky card + the other revealed cards back on TOP of the
      --     Summit deck.
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> ReturnToTheShoreline <$> liftRunMessage msg attrs
