module Arkham.Asset.Cards.SledDog (sledDog, SledDog (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers (getCanMoveToLocations)
import Arkham.Modifier
import Arkham.Movement

newtype SledDog = SledDog AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sledDog :: AssetCard SledDog
sledDog = ally SledDog Cards.sledDog (2, 2)

instance HasModifiersFor SledDog where
  getModifiersFor target (SledDog a) | a `is` target = do
    pure $ toModifiers a [SharesSlotWith 2 "Sled Dog"]
  getModifiersFor _ _ = pure []

instance HasAbilities SledDog where
  getAbilities (SledDog a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility [#move] (ExhaustXAssetCost "Sled Dog")
    , restrictedAbility a 2 ControlsThis $ ActionAbility [#fight] (ExhaustXAssetCost "Sled Dog")
    ]

getExhaustedCount :: Payment -> Int
getExhaustedCount p = go p
 where
  go (Payments ps) = sum (map go ps)
  go (ExhaustPayment xs) = length xs
  go _ = 0

instance RunMessage SledDog where
  runMessage msg a@(SledDog attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (getExhaustedCount -> x) -> do
      push $ DoStep x msg
      pure a
    DoStep n msg'@(UseCardAbility iid (isSource attrs -> True) 1 _ _) | n > 0 -> do
      locations <- getCanMoveToLocations iid (attrs.ability 1)
      unless (null locations) $ do
        chooseOne
          iid
          [ targetLabel location [MoveTo $ move (toSource attrs) iid location, DoStep (n - 1) msg']
          | location <- locations
          ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (getExhaustedCount -> x) -> do
      skillTestModifiers (attrs.ability 2) iid [SkillModifier #combat x, NoStandardDamage, DamageDealt x]
      chooseFightEnemy iid (attrs.ability 2)
      pure a
    _ -> SledDog <$> liftRunMessage msg attrs
