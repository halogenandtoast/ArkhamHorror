module Arkham.Asset.Cards.VaultOfKnowledge
  ( vaultOfKnowledge
  , VaultOfKnowledge(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype VaultOfKnowledge = VaultOfKnowledge AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultOfKnowledge :: AssetCard VaultOfKnowledge
vaultOfKnowledge = asset VaultOfKnowledge Cards.vaultOfKnowledge

instance HasModifiersFor VaultOfKnowledge where
  getModifiersFor (InvestigatorTarget iid) (VaultOfKnowledge a)
    | a `controlledBy` iid = pure $ toModifiers a [HandSize 2]
  getModifiersFor _ _ = pure []

instance HasAbilities VaultOfKnowledge where
  getAbilities (VaultOfKnowledge a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (SkillTestResult Timing.After You (WhileInvestigating Anywhere)
            $ SuccessResult AnyValue
            )
        $ ExhaustCost (toTarget a)
    ]

instance RunMessage VaultOfKnowledge where
  runMessage msg a@(VaultOfKnowledge attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      iids <- selectList $ colocatedWith iid
      push $ chooseOrRunOne
        iid
        [ targetLabel iid' [drawCards iid' attrs 1] | iid' <- iids ]
      pure a
    _ -> VaultOfKnowledge <$> runMessage msg attrs
