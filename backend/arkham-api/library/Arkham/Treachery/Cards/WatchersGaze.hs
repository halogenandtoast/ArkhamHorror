module Arkham.Treachery.Cards.WatchersGaze (watchersGaze, WatchersGaze (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WatchersGaze = WatchersGaze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGaze :: TreacheryCard WatchersGaze
watchersGaze = treachery WatchersGaze Cards.watchersGaze

instance RunMessage WatchersGaze where
  runMessage msg t@(WatchersGaze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigatorsWithRevelers <- select $ HasMatchingAsset $ assetIs Assets.innocentReveler
      sids <- getRandoms
      for_ (zip sids (nub $ iid : investigatorsWithRevelers)) \(sid, iid') ->
        revelationSkillTest sid iid' attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) (DamageFirst Assets.innocentReveler) 0 1
      pure t
    _ -> WatchersGaze <$> liftRunMessage msg attrs
