module Arkham.Homebrew.AgesUnwound.Treacheries.RiggedToBlow (riggedToBlow) where

import Arkham.Helpers.Location (getLocationOf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers (
  canMoveToConnecting,
  moveToConnecting,
 )
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

data Meta = Meta {drawnAt :: Maybe LocationId, moveEach :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RiggedToBlow = RiggedToBlow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riggedToBlow :: TreacheryCard RiggedToBlow
riggedToBlow = treacheryWith RiggedToBlow Cards.riggedToBlow (setMeta $ Meta Nothing False)

instance RunMessage RiggedToBlow where
  runMessage msg t@(RiggedToBlow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mlid <- getLocationOf iid
      for_ mlid \_ -> do
        sid <- getRandom
        chooseOneM iid $ withI18n do
          chooseTest #agility 2 do
            push $ HandleAbilityOption iid (toSource attrs) 1
            revelationSkillTest sid iid attrs #agility (Fixed 2)
          chooseTest #agility 4 do
            push $ HandleAbilityOption iid (toSource attrs) 2
            revelationSkillTest sid iid attrs #agility (Fixed 4)
        doStep 1 msg
      pure $ RiggedToBlow $ setMeta (Meta mlid False) attrs
    HandleAbilityOption _ (isSource attrs -> True) n -> do
      let m = toResult @Meta attrs.meta
      pure $ RiggedToBlow $ setMeta (m {moveEach = n == 2}) attrs
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      here <- select $ investigatorAt (locationWithInvestigator iid)
      canMove <- canMoveToConnecting iid
      when (notNull here && canMove) do
        chooseOneM iid $ campaignI18n do
          if moveEach (toResult @Meta attrs.meta)
            then labeled "riggedToBlow.moveEach" do
              for_ here $ moveToConnecting attrs iid
            else labeled "riggedToBlow.moveOne" do
              if length here == 1
                then for_ here $ moveToConnecting attrs iid
                else chooseTargetM iid here $ moveToConnecting attrs iid
          unscoped $ labeled "doNotMoveToConnecting" nothing
      pure t
    DoStep 1 (Revelation _ (isSource attrs -> True)) -> do
      for_ (drawnAt $ toResult @Meta attrs.meta) \lid ->
        selectEach (investigatorAt $ LocationWithId lid) \i -> assignDamage i attrs 2
      pure t
    _ -> RiggedToBlow <$> liftRunMessage msg attrs
