module Arkham.Asset.Cards.BrandOfCthugha1 (brandOfCthugha1, BrandOfCthugha1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype BrandOfCthugha1 = BrandOfCthugha1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brandOfCthugha1 :: AssetCard BrandOfCthugha1
brandOfCthugha1 = asset BrandOfCthugha1 Cards.brandOfCthugha1

instance HasAbilities BrandOfCthugha1 where
  getAbilities (BrandOfCthugha1 a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage BrandOfCthugha1 where
  runMessage msg a@(BrandOfCthugha1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel
            sType
            [ skillTestModifiers (attrs.ability 1) iid [SkillModifier sType 1, NoStandardDamage]
            , chooseFightEnemy iid (attrs.ability 1) sType
            ]
          | sType <- [#willpower, #combat]
          ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      pushWhen (n == 0) $ LoseActions iid (attrs.ability 1) 1
      case attrs.use Charge of
        0 -> pure ()
        1 -> push $ ResolveAmounts iid [("Charges", 1)] (toTarget attrs)
        _ -> do
          player <- getPlayer iid
          push
            $ chooseAmounts player "Amount of Charges to Spend" (MaxAmountTarget 2) [("Charges", (1, 2))] attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Charges" -> n) (isTarget attrs -> True) -> do
      pushAll
        [ skillTestModifier (attrs.ability 1) iid (DamageDealt n)
        , SpendUses (toTarget attrs) Charge n
        ]
      pure a
    _ -> BrandOfCthugha1 <$> runMessage msg attrs
