module Arkham.Act.Cards.ChildrenOfBlood.NewHorizons.BringDownTheBeast (bringDownTheBeast) where

import Arkham.Ability
import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher

newtype BringDownTheBeast = BringDownTheBeast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bringDownTheBeast :: ActCard BringDownTheBeast
bringDownTheBeast = act (3, A) BringDownTheBeast Cards.bringDownTheBeast Nothing

instance HasAbilities BringDownTheBeast where
  getAbilities = actAbilities \x ->
    [ onlyOnce $ restricted x 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    , mkAbility x 2
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny (EnemyWithTitle "Zburamoarte")
    ]

instance RunMessage BringDownTheBeast where
  runMessage msg a@(BringDownTheBeast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R2
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R3
      pure a
    _ -> BringDownTheBeast <$> liftRunMessage msg attrs
