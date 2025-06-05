module Arkham.Asset.Assets.JennysTwin45sAdvanced (jennysTwin45sAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Enemy.Types (Field (EnemyHealthActual))
import Arkham.Helpers.Calculation
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype JennysTwin45sAdvanced = JennysTwin45sAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jennysTwin45sAdvanced :: AssetCard JennysTwin45sAdvanced
jennysTwin45sAdvanced = asset JennysTwin45sAdvanced Cards.jennysTwin45sAdvanced

instance HasAbilities JennysTwin45sAdvanced where
  getAbilities (JennysTwin45sAdvanced a) =
    [ restricted a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1
    , restricted a 2 ControlsThis
        $ triggered
          (EnemyDefeated #after You (BySource $ SourceIs $ a.ability 1) EnemyWithHealth)
          (exhaust a)
    ]

instance RunMessage JennysTwin45sAdvanced where
  runMessage msg a@(JennysTwin45sAdvanced attrs) = runQueueT $ case msg of
    PaidForCardCost _ card payment | toCardId card == toCardId attrs -> do
      let n = totalResourcePayment payment
      JennysTwin45sAdvanced
        <$> liftRunMessage msg (attrs & printedUsesL .~ Uses Ammo (Fixed n) & tokensL .~ singletonMap Ammo n)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 2]
      chooseFightEnemy sid iid source
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (defeatedEnemy -> eid) _ -> do
      n <- fieldMapM EnemyHealthActual calculatePrinted eid
      gainResources iid (attrs.ability 2) n
      pure a
    _ -> JennysTwin45sAdvanced <$> liftRunMessage msg attrs
