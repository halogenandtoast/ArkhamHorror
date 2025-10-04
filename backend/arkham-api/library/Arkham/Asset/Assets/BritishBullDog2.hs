module Arkham.Asset.Assets.BritishBullDog2 (britishBullDog2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Fight
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BritishBullDog2 = BritishBullDog2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

britishBullDog2 :: AssetCard BritishBullDog2
britishBullDog2 = asset BritishBullDog2 Cards.britishBullDog2

instance HasModifiersFor BritishBullDog2 where
  getModifiersFor (BritishBullDog2 a) = for_ a.controller \iid -> do
    modifiedWhen_
      a
      (hasUses a)
      (AbilityTarget iid $ AbilityRef (toSource a) 1)
      [ canFightOverride
          $ EnemyWithoutModifier CannotBeAttacked
          <> oneOf
            [NonEliteEnemy <> at_ (connectedFrom $ locationWithInvestigator iid), enemyAtLocationWith iid]
      ]

instance HasAbilities BritishBullDog2 where
  getAbilities (BritishBullDog2 a) =
    [ controlled_ a 1 $ fightAction $ assetUseCost a Ammo 1
    , restricted a 2 InYourHand
        $ freeReaction
        $ SkillTestResult #after You (WhileParleyingWithAnEnemy AnyEnemy) #failure
    ]

instance RunMessage BritishBullDog2 where
  runMessage msg a@(BritishBullDog2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [AnySkillValue 2, DamageDealt 1, IgnoreAloof]
      fight <-
        mkChooseFightMatch sid iid (attrs.ability 1)
          $ CanFightEnemyWithOverride
          $ CriteriaOverride canFightIgnoreAloof
      chooseOneM iid do
        labeled "Use {agility}" $ push $ withSkillType #agility fight
        labeled "Use {combat}" $ push fight
      pure a
    InHand iid (UseThisAbility iid' (isSource attrs -> True) 2) | iid == iid' -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> BritishBullDog2 <$> liftRunMessage msg attrs
