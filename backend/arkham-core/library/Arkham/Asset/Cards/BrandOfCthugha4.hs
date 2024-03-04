module Arkham.Asset.Cards.BrandOfCthugha4 (brandOfCthugha4, BrandOfCthugha4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype BrandOfCthugha4 = BrandOfCthugha4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brandOfCthugha4 :: AssetCard BrandOfCthugha4
brandOfCthugha4 = asset BrandOfCthugha4 Cards.brandOfCthugha4

instance HasAbilities BrandOfCthugha4 where
  getAbilities (BrandOfCthugha4 a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage BrandOfCthugha4 where
  runMessage msg a@(BrandOfCthugha4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel
            sType
            [ skillTestModifiers (attrs.ability 1) iid [SkillModifier sType 2, NoStandardDamage]
            , chooseFightEnemy iid (attrs.ability 1) sType
            ]
          | sType <- [#willpower, #combat]
          ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      player <- getPlayer iid
      pushWhen (n == 0) $ LoseActions iid (toAbilitySource attrs 1) 2
      case attrs.use Charge of
        0 -> pure ()
        1 -> push $ ResolveAmounts iid [("Charges", 1)] (toTarget attrs)
        (min 3 -> x) ->
          push
            $ chooseAmounts player "Amount of Charges to Spend" (MaxAmountTarget x) [("Charges", (1, x))] attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Charges" -> n) (isTarget attrs -> True) -> do
      pushAll
        [ skillTestModifier (attrs.ability 1) iid (DamageDealt n)
        , SpendUses (toTarget attrs) Charge n
        ]
      pure a
    _ -> BrandOfCthugha4 <$> runMessage msg attrs
