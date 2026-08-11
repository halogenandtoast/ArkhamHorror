module Arkham.Homebrew.DarkMatter.Assets.LtArcherMichaels (ltArcherMichaels) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LtArcherMichaels = LtArcherMichaels AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ltArcherMichaels :: AssetCard LtArcherMichaels
ltArcherMichaels = asset LtArcherMichaels Cards.ltArcherMichaels

{- | "Revelation - Put this card into play under your control.
[action]: Deal 2 damage to any enemy in play. This action does not provoke
attacks of opportunity. (Group limit once per game.)"
-}
instance HasAbilities LtArcherMichaels where
  getAbilities (LtArcherMichaels a) =
    [groupLimit PerGame $ controlled_ a 1 $ actionAbilityWithCost mempty]

instance RunMessage LtArcherMichaels where
  runMessage msg a@(LtArcherMichaels attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select AnyEnemy
      chooseOneM iid $ targets enemies \enemy ->
        nonAttackEnemyDamage Nothing (attrs.ability 1) 2 enemy
      pure a
    _ -> LtArcherMichaels <$> liftRunMessage msg attrs
