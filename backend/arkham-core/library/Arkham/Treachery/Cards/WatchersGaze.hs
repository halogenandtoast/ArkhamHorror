module Arkham.Treachery.Cards.WatchersGaze (watchersGaze, WatchersGaze (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WatchersGaze = WatchersGaze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGaze :: TreacheryCard WatchersGaze
watchersGaze = treachery WatchersGaze Cards.watchersGaze

instance RunMessage WatchersGaze where
  runMessage msg t@(WatchersGaze attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      innocentRevelerIds <- select $ assetControlledBy iid <> assetIs Assets.innocentReveler
      investigatorsWithRevelers <-
        catMaybes <$> traverse (selectOne . HasMatchingAsset . AssetWithId) innocentRevelerIds
      sid <- getRandom
      pushAll
        [ revelationSkillTest sid iid' attrs #willpower (Fixed 4)
        | iid' <- nub (iid : investigatorsWithRevelers)
        ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) (DamageFirst Assets.innocentReveler) 0 1
      pure t
    _ -> WatchersGaze <$> runMessage msg attrs
