module Arkham.Asset.Assets.VaultOfKnowledge (
  vaultOfKnowledge,
  VaultOfKnowledge (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype VaultOfKnowledge = VaultOfKnowledge AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultOfKnowledge :: AssetCard VaultOfKnowledge
vaultOfKnowledge = asset VaultOfKnowledge Cards.vaultOfKnowledge

instance HasModifiersFor VaultOfKnowledge where
  getModifiersFor (VaultOfKnowledge a) = controllerGets a [HandSize 2]

instance HasAbilities VaultOfKnowledge where
  getAbilities (VaultOfKnowledge a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (SkillTestResult Timing.After You (WhileInvestigating Anywhere) $ SuccessResult AnyValue)
          (exhaust a)
    ]

instance RunMessage VaultOfKnowledge where
  runMessage msg a@(VaultOfKnowledge attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      investigators <-
        map (\iid' -> (iid', drawCards iid' (toAbilitySource attrs 1) 1))
          <$> select (affectsOthers $ colocatedWith iid)
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [targetLabel iid' [drawing] | (iid', drawing) <- investigators]
      pure a
    _ -> VaultOfKnowledge <$> runMessage msg attrs
