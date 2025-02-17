module Arkham.Helpers.Healing where

import Arkham.Classes.HasQueue hiding (fromQueue)
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Message (Message (..), MessageType (..), messageType)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Source
import Control.Monad.Trans.Class

chooseHealDamageOrHorror
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
chooseHealDamageOrHorror source iid = do
  chooseOrRunOneM iid do
    whenM (canHaveDamageHealed source iid) do
      damageLabeled iid do
        healDamage iid source 1
    whenM (canHaveHorrorHealed source iid) do
      horrorLabeled iid do
        healHorror iid source 1

getDamageAmounts :: (MonadTrans t, HasQueue Message m) => InvestigatorId -> t m (Int, Int)
getDamageAmounts iid = fromQueue \queue -> case dropUntilDamage queue of
  dmsg : _ -> case dmsg of
    InvestigatorDamage iid' _ damage' horror' | iid' == iid -> (damage', horror')
    InvestigatorDoAssignDamage iid' _ _ _ damage' horror' _ _ | iid' == iid -> (damage', horror')
    _ -> error "mismatch"
  _ -> error "unhandled"
 where
  dropUntilDamage = dropWhile (notElem DamageMessage . messageType)

getAssetDamageAmounts :: (MonadTrans t, HasQueue Message m) => AssetId -> t m (Int, Int)
getAssetDamageAmounts aid = fromQueue \queue -> case dropUntilDamage queue of
  dmsg : _ -> case dmsg of
    DealAssetDamageWithCheck aid' _ damage' horror' _ | aid' == aid -> (damage', horror')
    DealAssetDirectDamage aid' _ damage' horror' | aid' == aid -> (damage', horror')
    AssignAssetDamageWithCheck aid' _ damage' horror' _ | aid' == aid -> (damage', horror')
    _ -> error "mismatch"
  _ -> error "unhandled"
 where
  dropUntilDamage = dropWhile (notElem DamageMessage . messageType)
