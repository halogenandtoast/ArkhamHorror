module Arkham.Act.Cards.AllIn (allIn) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Projection

newtype AllIn = AllIn ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIn :: ActCard AllIn
allIn = act (3, A) AllIn Cards.allIn Nothing

instance HasAbilities AllIn where
  getAbilities = actAbilities \x ->
    [ skillTestAbility
        $ restricted
          (proxied (assetIs Assets.drFrancisMorgan) x)
          1
          (Uncontrolled <> OnSameLocation)
          parleyAction_
    , restricted x 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage AllIn where
  runMessage msg a@(AllIn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      resignedWithDrFrancisMorgan <- resignedWith Assets.drFrancisMorgan
      push $ if resignedWithDrFrancisMorgan then R2 else R1
      pure a
    UseThisAbility iid (ProxySource (AssetSource aid) (isSource attrs -> True)) 1 | onSide A attrs -> do
      sid <- getRandom
      parley sid iid attrs aid #willpower (Fixed 3)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) | onSide A attrs -> do
      aid <- selectJust $ assetIs Assets.drFrancisMorgan
      currentClueCount <- field AssetClues aid
      requiredClueCount <- perPlayer 1
      placeClues (attrs.ability 1) aid 1
      when (currentClueCount + 1 >= requiredClueCount) $ takeControlOfAsset iid aid
      pure a
    _ -> AllIn <$> liftRunMessage msg attrs
