module Arkham.Act.Cards.AscendTheMountain (ascendTheMountain) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Expedition))

newtype AscendTheMountain = AscendTheMountain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendTheMountain :: ActCard AscendTheMountain
ascendTheMountain = act (1, A) AscendTheMountain Cards.ascendTheMountain Nothing

instance HasAbilities AscendTheMountain where
  getAbilities (AscendTheMountain x) =
    extend
      x
      [ restricted x 1 (exists $ AssetAt YourLocation <> withTrait Expedition) actionAbility
      , restricted x 2 AllUndefeatedInvestigatorsResigned
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage AscendTheMountain where
  runMessage msg a@(AscendTheMountain attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ AssetAt (locationWithInvestigator iid) <> withTrait Expedition
      chooseTargetM iid assets $ takeControlOfAsset iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> AscendTheMountain <$> liftRunMessage msg attrs
