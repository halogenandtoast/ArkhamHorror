module Arkham.Asset.Cards.LiquidCourage (
  liquidCourage,
  LiquidCourage (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.SkillType

newtype LiquidCourage = LiquidCourage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

liquidCourage :: AssetCard LiquidCourage
liquidCourage = asset LiquidCourage Cards.liquidCourage

instance HasAbilities LiquidCourage where
  getAbilities (LiquidCourage x) =
    [ controlledAbility
        x
        1
        (exists (HealableInvestigator (toSource x) HorrorType $ InvestigatorAt YourLocation))
        $ actionAbilityWithCost (assetUseCost x Supply 1)
    ]

instance RunMessage LiquidCourage where
  runMessage msg a@(LiquidCourage attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      iids <- selectList $ HealableInvestigator (toSource attrs) HorrorType $ colocatedWith iid
      player <- getPlayer iid
      pushIfAny iids
        $ chooseOrRunOne
          player
          [ targetLabel
            iid'
            [ HealHorrorWithAdditional (toTarget iid') (toSource attrs) 1
            , beginSkillTest iid' source iid' SkillWillpower 2
            ]
          | iid' <- iids
          ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ AdditionalHealHorror (toTarget iid) (toSource attrs) 1
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      pushAll
        [ AdditionalHealHorror (toTarget iid) (toSource attrs) 0
        , toMessage $ randomDiscard iid (toSource attrs)
        ]
      pure a
    _ -> LiquidCourage <$> runMessage msg attrs
