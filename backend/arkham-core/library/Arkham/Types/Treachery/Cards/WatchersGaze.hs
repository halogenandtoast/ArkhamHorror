module Arkham.Types.Treachery.Cards.WatchersGaze
  ( watchersGaze
  , WatchersGaze(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype WatchersGaze = WatchersGaze TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGaze :: TreacheryCard WatchersGaze
watchersGaze = treachery WatchersGaze Cards.watchersGaze

instance HasModifiersFor env WatchersGaze

instance HasActions env WatchersGaze where
  getActions i window (WatchersGaze attrs) = getActions i window attrs

instance
  ( HasId (Maybe OwnerId) env AssetId
  , HasSet AssetId env (InvestigatorId, CardDef)
  , TreacheryRunner env
  )
  => RunMessage env WatchersGaze where
  runMessage msg t@(WatchersGaze attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      innocentRevelerIds <- getSetList @AssetId (iid, Assets.innocentReveler)
      investigatorsWithRevelers <-
        catMaybes
          <$> traverse (fmap (fmap unOwnerId) . getId) innocentRevelerIds
      t <$ pushAll
        ([ RevelationSkillTest iid' source SkillWillpower 4
         | iid' <- nub (iid : investigatorsWithRevelers)
         ]
        <> [Discard (toTarget attrs)]
        )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        t <$ push
          (InvestigatorAssignDamage
            iid
            source
            (DamageFirst Assets.innocentReveler)
            0
            1
          )
    _ -> WatchersGaze <$> runMessage msg attrs
