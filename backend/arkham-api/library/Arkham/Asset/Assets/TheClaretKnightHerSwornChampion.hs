module Arkham.Asset.Assets.TheClaretKnightHerSwornChampion (theClaretKnightHerSwornChampion) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.DogsOfWar.Helpers

newtype TheClaretKnightHerSwornChampion = TheClaretKnightHerSwornChampion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theClaretKnightHerSwornChampion :: AssetCard TheClaretKnightHerSwornChampion
theClaretKnightHerSwornChampion = allyWith TheClaretKnightHerSwornChampion Cards.theClaretKnightHerSwornChampion (4, 2) noSlots

instance HasAbilities TheClaretKnightHerSwornChampion where
  getAbilities (TheClaretKnightHerSwornChampion a) =
    [ controlled a 1 (youExist InvestigatorWithAnyResources) $ FastAbility (exhaust a)
    , controlled a 2 (thisExists a (AssetWithTokens (atLeast 1) #resource)) $ FastAbility (exhaust a)
    ]

instance RunMessage TheClaretKnightHerSwornChampion where
  runMessage msg a@(TheClaretKnightHerSwornChampion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- field InvestigatorResources iid
      scenarioI18n
        $ chooseAmount'
          iid
          "theClaretKnight.resources"
          "$resources"
          1
          (min 3 resources)
          (IndexedTarget 1 $ toTarget attrs)
      pure a
    ResolveAmounts iid (getChoiceAmount "$resources" -> n) (IndexedTarget 1 (isTarget attrs -> True)) -> do
      moveTokens (attrs.ability 1) iid attrs #resource n
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scenarioI18n
        $ chooseAmount'
          iid
          "theClaretKnight.resources"
          "$resources"
          1
          (min 3 $ attrs.token #resource)
          (IndexedTarget 2 $ toTarget attrs)
      pure a
    ResolveAmounts _iid (getChoiceAmount "$resources" -> n) (IndexedTarget 2 (isTarget attrs -> True)) -> do
      doStep n msg
      pure a
    DoStep n msg'@(ResolveAmounts iid _ (IndexedTarget 2 (isTarget attrs -> True))) | n > 0 -> do
      assets <- select $ assetControlledBy iid <> mapOneOf AssetCanHaveUses [Charge, Ammo, Supply, Secret]
      if null assets
        then moveTokens (attrs.ability 1) attrs iid #resource n
        else do
          chooseOneM iid do
            targeting iid $ moveTokens (attrs.ability 2) attrs iid #resource 1
            targets assets \aid -> do
              removeTokens (attrs.ability 2) attrs #resource 1
              handleTarget iid (attrs.ability 2) aid
          doStep (n - 1) msg'
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      let whenCanHoldToken tkn = whenM (matches aid (AssetCanHaveUses tkn))
      chooseOrRunOneM iid $ withI18n do
        whenCanHoldToken Charge $ labeled' "charge" $ addUses attrs aid Charge 1
        whenCanHoldToken Ammo $ labeled' "ammo" $ addUses attrs aid Ammo 1
        whenCanHoldToken Supply $ labeled' "supply" $ addUses attrs aid Supply 1
        whenCanHoldToken Secret $ labeled' "secret" $ addUses attrs aid Secret 1
      pure a
    _ -> TheClaretKnightHerSwornChampion <$> liftRunMessage msg attrs
