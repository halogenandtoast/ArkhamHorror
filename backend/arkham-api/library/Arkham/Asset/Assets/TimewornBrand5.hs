module Arkham.Asset.Assets.TimewornBrand5 (timewornBrand5, TimewornBrand5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Elite))

newtype TimewornBrand5 = TimewornBrand5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timewornBrand5 :: AssetCard TimewornBrand5
timewornBrand5 = asset TimewornBrand5 Cards.timewornBrand5

instance HasAbilities TimewornBrand5 where
  getAbilities (TimewornBrand5 a) =
    [ (cardI18n $ withI18nTooltip "timewornBrand5.actionIfTimeworn")
        $ controlled a 1 (thisIs a $ asset_ #ready) fightAction_
    , (cardI18n $ withI18nTooltip "timewornBrand5.actionExhaustTimeworn")
        $ limitedAbility (MaxPer Cards.timewornBrand5 PerGame 1)
        $ restricted a 2 ControlsThis
        $ fightAction (exhaust a)
    ]

instance RunMessage TimewornBrand5 where
  runMessage msg a@(TimewornBrand5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      skillTestModifiers sid source iid [AddSkillValue #willpower, DamageDealt 3]
      chooseFightEnemy sid iid source
      pure a
    Defeated (EnemyTarget _) _ (isAbilitySource attrs 2 -> True) traits | Elite `elem` traits -> do
      for_ attrs.controller \iid -> drawCardsIfCan iid (attrs.ability 2) 3
      pure a
    _ -> TimewornBrand5 <$> liftRunMessage msg attrs
