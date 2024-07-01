module Arkham.Asset.Cards.EnchantedBow2 (enchantedBow2, EnchantedBow2 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype EnchantedBow2 = EnchantedBow2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBow2 :: AssetCard EnchantedBow2
enchantedBow2 = asset EnchantedBow2 Cards.enchantedBow2

instance HasModifiersFor EnchantedBow2 where
  getModifiersFor (AbilityTarget iid ability) (EnchantedBow2 a) | ability.source.asset == Just a.id && ability.index == 1 = do
    let meta = toResultDefault True a.meta
    modified
      a
      [ CanModify
        $ EnemyFightActionCriteria
        $ CriteriaOverride
        $ EnemyCriteria
        $ ThisEnemy
        $ EnemyWithoutModifier CannotBeAttacked
        <> oneOf
          [NonEliteEnemy <> EnemyAt (ConnectedFrom $ locationWithInvestigator iid), enemyAtLocationWith iid]
      | hasUses a && meta
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities EnchantedBow2 where
  getAbilities (EnchantedBow2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ fightAction
        $ exhaust a
        <> CostIfEnemy
          (EnemyAt YourLocation <> CanBeAttackedBy You)
          ( CostWhenEnemy
              ( NonEliteEnemy
                  <> EnemyWithoutModifier CannotBeAttacked
                  <> EnemyAt (ConnectedFrom (LocationWithInvestigator $ HasMatchingAsset (be a)))
              )
              (UpTo 1 (assetUseCost a Charge 1))
          )
          (assetUseCost a Charge 1)
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
      let fight = if n > 0 then mkChooseFightMatch iid source override else mkChooseFight iid source
      skillTestModifiers source iid
        $ [AnySkillValue 1, DamageDealt 1]
        <> (guard (n > 0) *> [IgnoreAloof, IgnoreRetaliate])

      fightWillpower <- leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) fight
      fightAgility <- leftOr <$> aspect iid source (#agility `InsteadOf` #combat) fight

      chooseOne
        iid
        [Label "Use {willpower}" fightWillpower, Label "Use {agility}" fightAgility]
      pure . EnchantedBow2 $ setMeta (n > 0) attrs
    ResolvedAbility ab | isSource attrs ab.source -> do
      pure . EnchantedBow2 $ setMeta True attrs
    _ -> EnchantedBow2 <$> lift (runMessage msg attrs)
