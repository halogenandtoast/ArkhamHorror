module Arkham.Asset.Cards.LiquidCourage1 (liquidCourage1, LiquidCourage1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.Prelude

newtype LiquidCourage1 = LiquidCourage1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage1 :: AssetCard LiquidCourage1
liquidCourage1 = asset LiquidCourage1 Cards.liquidCourage1

instance HasAbilities LiquidCourage1 where
  getAbilities (LiquidCourage1 x) =
    [ withCriteria (mkAbility x 1 $ ActionAbility [] $ ActionCost 1 <> assetUseCost x Supply 1)
        $ ControlsThis
        <> InvestigatorExists
          ( HealableInvestigator (toSource x) HorrorType
              $ InvestigatorAt YourLocation
          )
    ]

instance RunMessage LiquidCourage1 where
  runMessage msg a@(LiquidCourage1 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      iids <- select $ HealableInvestigator (toAbilitySource attrs 1) HorrorType $ colocatedWith iid
      player <- getPlayer iid
      sid <- getRandom
      pushWhen (notNull iids)
        $ chooseOrRunOne player
        $ [ targetLabel iid'
            $ [ HealHorrorWithAdditional (toTarget iid') (toSource attrs) 1
              , beginSkillTest sid iid' (attrs.ability 1) iid' #willpower (Fixed 2)
              ]
          | iid' <- iids
          ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let drawing = drawCards iid (toAbilitySource attrs 1) 1
      pushAll [AdditionalHealHorror (toTarget iid) (toAbilitySource attrs 1) 0, drawing]
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      pushAll
        [ AdditionalHealHorror (toTarget iid) (toAbilitySource attrs 1) 1
        , toMessage $ randomDiscard iid (toSource attrs)
        ]
      pure a
    _ -> LiquidCourage1 <$> runMessage msg attrs
