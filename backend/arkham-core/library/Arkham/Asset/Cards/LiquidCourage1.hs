module Arkham.Asset.Cards.LiquidCourage1
  ( liquidCourage1
  , LiquidCourage1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.SkillType

newtype LiquidCourage1 = LiquidCourage1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage1 :: AssetCard LiquidCourage1
liquidCourage1 = asset LiquidCourage1 Cards.liquidCourage1

instance HasAbilities LiquidCourage1 where
  getAbilities (LiquidCourage1 x) =
    [ restrictedAbility
          x
          1
          (ControlsThis <> InvestigatorExists
            (HealableInvestigator (toSource x) HorrorType
            $ InvestigatorAt YourLocation
            )
          )
        $ ActionAbility Nothing
        $ ActionCost 1 <> UseCost (AssetWithId $ toId x) Supply 1
    ]

instance RunMessage LiquidCourage1 where
  runMessage msg a@(LiquidCourage1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      iids <-
        selectList
        $ HealableInvestigator (toSource attrs) HorrorType
        $ colocatedWith iid
      when (notNull iids) $ push $ chooseOrRunOne
        iid
        [ targetLabel
            iid'
            [ HealHorrorWithAdditional (toTarget iid') (toSource attrs) 1
            , beginSkillTest iid' source iid' SkillWillpower 2
            ]
        | iid' <- iids
        ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        drawing <- drawCards iid (toSource attrs) 1
        pushAll [AdditionalHealHorror (toTarget iid) (toSource attrs) 0, drawing]
        pure a
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        pushAll
          [ AdditionalHealHorror (toTarget iid) (toSource attrs) 1
          , toMessage $ randomDiscard iid (toSource attrs)
          ]
        pure a
    _ -> LiquidCourage1 <$> runMessage msg attrs
