module Arkham.Asset.Assets.SledDog (sledDog, SledDog (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers (getCanMoveToLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Movement

newtype SledDog = SledDog AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sledDog :: AssetCard SledDog
sledDog = ally SledDog Cards.sledDog (2, 2)

instance HasModifiersFor SledDog where
  getModifiersFor (SledDog a) = modifySelf a [SharesSlotWith 2 "Sled Dog"]

instance HasAbilities SledDog where
  getAbilities (SledDog a) =
    [ restricted a 1 ControlsThis
        $ ActionAbility [#move] (ActionCost 1 <> ExhaustXAssetCost "Sled Dog")
    , restricted a 2 ControlsThis
        $ ActionAbility [#fight] (ActionCost 1 <> ExhaustXAssetCost "Sled Dog")
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
          [ targetLabel location [Move $ move (toSource attrs) iid location, DoStep (n - 1) msg']
          | location <- locations
          ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ (getExhaustedCount -> x) -> do
      sid <- getRandom
      skillTestModifiers
        sid
        (attrs.ability 2)
        iid
        [SkillModifier #combat x, NoStandardDamage, DamageDealt x]
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    _ -> SledDog <$> liftRunMessage msg attrs
