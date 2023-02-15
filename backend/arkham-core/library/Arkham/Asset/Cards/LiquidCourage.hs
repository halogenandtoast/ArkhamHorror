module Arkham.Asset.Cards.LiquidCourage
  ( liquidCourage
  , LiquidCourage(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype LiquidCourage = LiquidCourage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage :: AssetCard LiquidCourage
liquidCourage = asset LiquidCourage Cards.liquidCourage

instance HasAbilities LiquidCourage where
  getAbilities (LiquidCourage x) =
    [ restrictedAbility
          x
          1
          (ControlsThis <> InvestigatorExists
            (HealableInvestigator (toSource x) HorrorType
            $ InvestigatorAt YourLocation
            )
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
    ]

instance RunMessage LiquidCourage where
  runMessage msg a@(LiquidCourage attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      iids <-
        selectList
        $ HealableInvestigator (toSource attrs) HorrorType
        $ colocatedWith iid
      when (notNull iids) $ push $ chooseOrRunOne
        iid
        [ targetLabel
            iid'
            [ HealHorrorWithAdditional (idToTarget iid') (toSource attrs) 1
            , beginSkillTest
              iid'
              source
              (idToTarget iid')
              Nothing
              SkillWillpower
              2
            ]
        | iid' <- iids
        ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _ -> do
      push $ AdditionalHealHorror (InvestigatorTarget iid) (toSource attrs) 1
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _ -> do
      pushAll
        [ AdditionalHealHorror (InvestigatorTarget iid) (toSource attrs) 0
        , RandomDiscard iid (toSource attrs) AnyCard
        ]
      pure a
    _ -> LiquidCourage <$> runMessage msg attrs
