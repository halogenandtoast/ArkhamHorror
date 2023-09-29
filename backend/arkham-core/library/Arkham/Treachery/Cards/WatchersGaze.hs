module Arkham.Treachery.Cards.WatchersGaze (
  watchersGaze,
  WatchersGaze (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WatchersGaze = WatchersGaze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGaze :: TreacheryCard WatchersGaze
watchersGaze = treachery WatchersGaze Cards.watchersGaze

instance RunMessage WatchersGaze where
  runMessage msg t@(WatchersGaze attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      innocentRevelerIds <-
        selectList
          (AssetControlledBy You <> assetIs Assets.innocentReveler)
      investigatorsWithRevelers <-
        catMaybes
          <$> traverse
            (selectOne . HasMatchingAsset . AssetWithId)
            innocentRevelerIds
      t
        <$ pushAll
          [ RevelationSkillTest iid' source SkillWillpower 4
          | iid' <- nub (iid : investigatorsWithRevelers)
          ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push
            $ InvestigatorAssignDamage
              iid
              source
              (DamageFirst Assets.innocentReveler)
              0
              1
          pure t
    _ -> WatchersGaze <$> runMessage msg attrs
