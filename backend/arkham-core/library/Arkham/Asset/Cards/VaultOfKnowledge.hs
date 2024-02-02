module Arkham.Asset.Cards.VaultOfKnowledge (
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
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

vaultOfKnowledge :: AssetCard VaultOfKnowledge
vaultOfKnowledge = asset VaultOfKnowledge Cards.vaultOfKnowledge

instance HasModifiersFor VaultOfKnowledge where
  getModifiersFor (InvestigatorTarget iid) (VaultOfKnowledge a) | a `controlledBy` iid = do
    pure $ toModifiers a [HandSize 2]
  getModifiersFor _ _ = pure []

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
      iids <- selectList $ affectsOthers $ colocatedWith iid
      investigators <- forToSnd iids $ \iid' -> drawCards iid' (toAbilitySource attrs 1) 1
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [targetLabel iid' [drawing] | (iid', drawing) <- investigators]
      pure a
    _ -> VaultOfKnowledge <$> runMessage msg attrs
