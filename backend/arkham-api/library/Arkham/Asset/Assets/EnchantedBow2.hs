module Arkham.Asset.Assets.EnchantedBow2 (enchantedBow2, EnchantedBow2 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Fight
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype EnchantedBow2 = EnchantedBow2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBow2 :: AssetCard EnchantedBow2
enchantedBow2 = asset EnchantedBow2 Cards.enchantedBow2

instance HasModifiersFor EnchantedBow2 where
  getModifiersFor (EnchantedBow2 a) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      selectOne (AbilityIs (toSource a) 1) >>= \case
        Nothing -> pure mempty
        Just ab -> do
          let meta = toResultDefault True a.meta
          modifiedWhen_
            a
            (hasUses a && meta)
            (AbilityTarget iid ab)
            [ CanModify
                $ EnemyFightActionCriteria
                $ CriteriaOverride
                $ EnemyCriteria
                $ ThisEnemy
                $ EnemyWithoutModifier CannotBeAttacked
                <> oneOf
                  [NonEliteEnemy <> EnemyAt (ConnectedFrom $ locationWithInvestigator iid), enemyAtLocationWith iid]
            ]

instance HasAbilities EnchantedBow2 where
  getAbilities (EnchantedBow2 a) =
    [ withAdditionalCost
        ( CostIfEnemy
            (EnemyAt YourLocation <> CanBeAttackedBy You)
            ( CostWhenEnemy
                ( NonEliteEnemy
                    <> EnemyWithoutModifier CannotBeAttacked
                    <> EnemyAt (ConnectedFrom (LocationWithInvestigator $ HasMatchingAsset (be a)))
                )
                (UpTo (Fixed 1) (assetUseCost a Charge 1))
            )
            (assetUseCost a Charge 1)
        )
        $ restrictedAbility a 1 ControlsThis
        $ fightAction
        $ exhaust a
    ]

instance RunMessage EnchantedBow2 where
  runMessage msg (EnchantedBow2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      let source = attrs.ability 1
      let override =
            CanFightEnemyWithOverride
              $ CriteriaOverride
              $ EnemyCriteria
              $ ThisEnemy
              $ NonEliteEnemy
              <> EnemyAt (ConnectedFrom (locationWithInvestigator iid))
      sid <- getRandom
      let fight = if n > 0 then mkChooseFightMatch sid iid source override else mkChooseFight sid iid source
      skillTestModifiers sid source iid
        $ [AnySkillValue 1, DamageDealt 1]
        <> (guard (n > 0) *> [IgnoreAloof, IgnoreRetaliate])

      chooseOneM iid do
        labeled "Use {willpower}" do
          aspect iid source (#willpower `InsteadOf` #combat) fight
        labeled "Use {agility}" do
          aspect iid source (#agility `InsteadOf` #combat) fight
      pure . EnchantedBow2 $ setMeta (n > 0) attrs
    ResolvedAbility ab | isSource attrs ab.source -> do
      pure . EnchantedBow2 $ setMeta True attrs
    _ -> EnchantedBow2 <$> liftRunMessage msg attrs
