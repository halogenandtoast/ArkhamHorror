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
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype LiquidCourage = LiquidCourage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage :: AssetCard LiquidCourage
liquidCourage = asset LiquidCourage Cards.liquidCourage

instance HasAbilities LiquidCourage where
  getAbilities (LiquidCourage x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> InvestigatorExists
          (InvestigatorAt YourLocation <> InvestigatorWithAnyHorror)
        )
        (ActionAbility Nothing $ Costs [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1]
        )
    ]

instance AssetRunner env => RunMessage env LiquidCourage where
  runMessage msg a@(LiquidCourage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      iids <- getSetList @InvestigatorId lid
      let
        doAbilityEffect iid' =
          [ HealHorror (InvestigatorTarget iid') 1
          , BeginSkillTest
            iid'
            source
            (InvestigatorTarget iid')
            Nothing
            SkillWillpower
            2
          ]
      a <$ case iids of
        [] -> pure ()
        [iid'] -> pushAll $ doAbilityEffect iid'
        _ -> push
          (chooseOne
            iid
            [ TargetLabel (InvestigatorTarget iid') (doAbilityEffect iid')
            | iid' <- iids
            ]
          )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a
      <$ push (HealHorror (InvestigatorTarget iid) 1)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (RandomDiscard iid)
    _ -> LiquidCourage <$> runMessage msg attrs
