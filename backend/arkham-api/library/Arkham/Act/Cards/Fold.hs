module Arkham.Act.Cards.Fold (fold) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (fold)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Projection

newtype Fold = Fold ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fold :: ActCard Fold
fold = act (3, A) Fold Cards.fold Nothing

instance HasAbilities Fold where
  getAbilities = actAbilities \x ->
    [ skillTestAbility
        $ restricted (proxied (assetIs Cards.peterClover) x) 1 (Uncontrolled <> OnSameLocation) parleyAction_
    , restricted x 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage Fold where
  runMessage msg a@(Fold attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      resignedWithPeterClover <- resignedWith Cards.peterClover
      push $ if resignedWithPeterClover then R3 else R1
      pure a
    UseThisAbility iid (ProxySource (AssetSource aid) (isSource attrs -> True)) 1 | onSide A attrs -> do
      sid <- getRandom
      parley sid iid attrs aid #willpower (Fixed 3)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      aid <- selectJust $ assetIs Cards.peterClover
      currentClueCount <- field AssetClues aid
      requiredClueCount <- perPlayer 1
      placeClues (attrs.ability 1) aid 1
      when (currentClueCount + 1 >= requiredClueCount) $ takeControlOfAsset iid aid
      pure a
    _ -> Fold <$> liftRunMessage msg attrs
