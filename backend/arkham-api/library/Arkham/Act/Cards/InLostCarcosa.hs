module Arkham.Act.Cards.InLostCarcosa (inLostCarcosa) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher

newtype InLostCarcosa = InLostCarcosa ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inLostCarcosa :: ActCard InLostCarcosa
inLostCarcosa = act (1, A) InLostCarcosa Cards.inLostCarcosa Nothing

instance HasAbilities InLostCarcosa where
  getAbilities (InLostCarcosa x) =
    withBaseAbilities
      x
      [ mkAbility x 1
          $ Objective
          $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 2) Anywhere)
      | onSide A x
      ]

instance RunMessage InLostCarcosa where
  runMessage msg a@(InLostCarcosa attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      theManInThePallidMask <- getSetAsideCard Enemies.theManInThePallidMask
      palaceOfTheKing <- getJustLocationByName "Palace of the King"
      createEnemyAt_ theManInThePallidMask palaceOfTheKing
      advanceActDeck attrs
      pure a
    _ -> InLostCarcosa <$> liftRunMessage msg attrs
