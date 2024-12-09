module Arkham.Asset.Assets.HeaddressOfYhaNthlei (
  headdressOfYhaNthlei,
  HeaddressOfYhaNthlei (..),
)
where

import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest
import Arkham.Matcher

newtype HeaddressOfYhaNthlei = HeaddressOfYhaNthlei AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

headdressOfYhaNthlei :: AssetCard HeaddressOfYhaNthlei
headdressOfYhaNthlei = asset HeaddressOfYhaNthlei Cards.headdressOfYhaNthlei

instance HasModifiersFor HeaddressOfYhaNthlei where
  getModifiersFor (HeaddressOfYhaNthlei a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      MaybeT getSkillTestAction >>= \case
        Action.Investigate -> do
          LocationTarget lid <- MaybeT getSkillTestTarget
          liftGuardM $ lid <=~> FloodedLocation
          pure [AnySkillValue 1]
        Action.Evade -> do
          EnemyTarget eid <- MaybeT getSkillTestTarget
          liftGuardM $ eid <=~> EnemyAt FloodedLocation
          pure [AnySkillValue 1]
        _ -> pure []

instance RunMessage HeaddressOfYhaNthlei where
  runMessage msg (HeaddressOfYhaNthlei attrs) = runQueueT $ case msg of
    _ -> HeaddressOfYhaNthlei <$> liftRunMessage msg attrs
