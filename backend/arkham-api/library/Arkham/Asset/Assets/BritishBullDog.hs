module Arkham.Asset.Assets.BritishBullDog (britishBullDog) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype BritishBullDog = BritishBullDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

britishBullDog :: AssetCard BritishBullDog
britishBullDog = asset BritishBullDog Cards.britishBullDog

instance HasAbilities BritishBullDog where
  getAbilities (BritishBullDog a) =
    [ controlled_ a 1 $ fightActionWithAlternate #agility $ assetUseCost a Ammo 1
    , restricted a 2 InYourHand
        $ freeReaction
        $ SkillTestResult #after You (WhileParleyingWithAnEnemy AnyEnemy) #failure
    ]

instance RunMessage BritishBullDog where
  runMessage msg a@(BritishBullDog attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      chooseFightEnemyWithSkillChoice sid iid (attrs.ability 1) [#combat, #agility]
      pure a
    InHand iid (UseThisAbility iid' (isSource attrs -> True) 2) | iid == iid' -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> BritishBullDog <$> liftRunMessage msg attrs
