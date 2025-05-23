module Arkham.Asset.Assets.ChicagoTypewriter4 (chicagoTypewriter4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype ChicagoTypewriter4 = ChicagoTypewriter4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chicagoTypewriter4 :: AssetCard ChicagoTypewriter4
chicagoTypewriter4 = asset ChicagoTypewriter4 Cards.chicagoTypewriter4

instance HasAbilities ChicagoTypewriter4 where
  getAbilities (ChicagoTypewriter4 a) =
    [ withAdditionalCost AdditionalActionsCost
        $ restricted a 1 ControlsThis
        $ fightAction
        $ assetUseCost a Ammo 1
    ]

getActionsSpent :: Payment -> Int
getActionsSpent (ActionPayment n) = n
getActionsSpent AdditionalActionPayment = 1
getActionsSpent (Payments ps) = sum $ map getActionsSpent ps
getActionsSpent _ = 0

instance RunMessage ChicagoTypewriter4 where
  runMessage msg a@(ChicagoTypewriter4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getActionsSpent -> actionsSpent) -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [DamageDealt 2, SkillModifier #combat (2 * actionsSpent)]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> ChicagoTypewriter4 <$> liftRunMessage msg attrs
