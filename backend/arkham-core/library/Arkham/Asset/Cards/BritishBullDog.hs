module Arkham.Asset.Cards.BritishBullDog (britishBullDog, BritishBullDog (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype BritishBullDog = BritishBullDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

britishBullDog :: AssetCard BritishBullDog
britishBullDog = asset BritishBullDog Cards.britishBullDog

instance HasAbilities BritishBullDog where
  getAbilities (BritishBullDog a) =
    [ restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1
    , restrictedAbility a 2 InYourHand
        $ freeReaction
        $ SkillTestResult #after You (WhileParleyingWithAnEnemy AnyEnemy) #failure
    ]

instance RunMessage BritishBullDog where
  runMessage msg a@(BritishBullDog attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
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
    _ -> BritishBullDog <$> liftRunMessage msg attrs
