module Arkham.Asset.Cards.BritishBullDog2 (britishBullDog2, BritishBullDog2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype BritishBullDog2 = BritishBullDog2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

britishBullDog2 :: AssetCard BritishBullDog2
britishBullDog2 = asset BritishBullDog2 Cards.britishBullDog2

instance HasAbilities BritishBullDog2 where
  getAbilities (BritishBullDog2 a) =
    [ restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1
    , restrictedAbility a 2 InYourHand
        $ freeReaction
        $ SkillTestResult #after You (WhileParleyingWithAnEnemy AnyEnemy) #failure
    ]

instance RunMessage BritishBullDog2 where
  runMessage msg a@(BritishBullDog2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [AnySkillValue 2, DamageDealt 1, IgnoreAloof]
      fight <- mkChooseFight sid iid (attrs.ability 1)
      chooseOne
        iid
        [ Label "Use {agility}" [toMessage $ withSkillType #agility fight]
        , Label "Use {combat}" [toMessage fight]
        ]
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> BritishBullDog2 <$> liftRunMessage msg attrs
