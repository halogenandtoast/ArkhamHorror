module Arkham.Asset.Assets.GavriellaMizrah (gavriellaMizrah) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype GavriellaMizrah = GavriellaMizrah AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellaMizrah :: AssetCard GavriellaMizrah
gavriellaMizrah = ally GavriellaMizrah Cards.gavriellaMizrah (4, 1)

instance HasModifiersFor GavriellaMizrah where
  getModifiersFor (GavriellaMizrah a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    [ controlled a 1 (CanDiscoverCluesAt YourLocation <> OnLocation LocationWithAnyClues)
        $ triggered (EnemyAttacksEvenIfCancelled #after You AnyEnemyAttack AnyEnemy) (exhaust a)
    ]

instance RunMessage GavriellaMizrah where
  runMessage msg a@(GavriellaMizrah attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> GavriellaMizrah <$> liftRunMessage msg attrs
