module Arkham.Treachery.Cards.DeadlyTorrent (deadlyTorrent) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevelFor)
import Arkham.ForMovement (ForMovement (ForMovement))
import Arkham.Location.FloodLevel (FloodLevel (Unflooded))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.SkillType
import Arkham.Slot (SlotType)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeadlyTorrent = DeadlyTorrent TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlyTorrent :: TreacheryCard DeadlyTorrent
deadlyTorrent = treachery DeadlyTorrent Cards.deadlyTorrent

instance RunMessage DeadlyTorrent where
  runMessage msg t@(DeadlyTorrent attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getFloodLevelFor iid >>= \case
        Unflooded -> gainSurge attrs
        _ -> do
          sid <- getRandom
          chooseOneM iid do
            skillLabeled #agility do
              push $ ForSkillType #agility msg
              revelationSkillTest sid iid attrs #agility (Fixed 4)
            skillLabeled #combat do
              push $ ForSkillType #combat msg
              revelationSkillTest sid iid attrs #combat (Fixed 4)
      pure t
    ForSkillType sType (Revelation _iid (isSource attrs -> True)) -> do
      pure $ DeadlyTorrent $ attrs & setMeta sType
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      when (toResult attrs.meta == SkillAgility) do
        locations <- select $ AccessibleFrom ForMovement (locationWithInvestigator iid)
        chooseOrRunOneM iid $ targets locations $ moveTo attrs iid
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      if toResult attrs.meta == SkillAgility
        then assignDamageAndHorror iid attrs 1 1
        else do
          assignDamage iid attrs 1
          chooseAndDiscardAssetMatching iid attrs
            $ assetControlledBy iid
            <> oneOf (map AssetWithSlot allSlots)
      pure t
    _ -> DeadlyTorrent <$> liftRunMessage msg attrs
   where
    allSlots = [minBound .. maxBound] :: [SlotType]
