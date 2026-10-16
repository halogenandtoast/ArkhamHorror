{- | "Revelation - Test {agility} (3). If you fail, take 1 damage and remove a
resource token from a non-weakness asset you control. If you fail and there is
an /Abomination/ enemy at your location, resolve the fail effect an additional
time."
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.SurgingFlesh (
  surgingFlesh,
) where

import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Abomination))
import Arkham.Treachery.Import.Lifted

newtype SurgingFlesh = SurgingFlesh TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

surgingFlesh :: TreacheryCard SurgingFlesh
surgingFlesh = treachery SurgingFlesh Cards.surgingFlesh

instance RunMessage SurgingFlesh where
  runMessage msg t@(SurgingFlesh attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      abomination <- selectAny $ enemyAtLocationWith iid <> EnemyWithTrait Abomination
      -- Each pass re-reads which assets still have a resource on them.
      doStep (if abomination then 2 else 1) msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      assignDamage iid attrs 1
      assets <-
        select
          $ NonWeaknessAsset
          <> assetControlledBy iid
          <> AssetWithTokens (atLeast 1) #resource
      chooseTargetM iid assets \aid -> removeTokens attrs aid #resource 1
      doNextStep msg'
      pure t
    _ -> SurgingFlesh <$> liftRunMessage msg attrs
