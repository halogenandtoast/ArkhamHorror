module Arkham.Homebrew.AgesUnwound.Treacheries.RiggedToBlow (riggedToBlow) where

import Arkham.ForMovement
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Treachery.Import.Lifted

{- | The option taken, and the location the card was drawn at -- the "Then"
clause damages everyone who was standing there, which the move may have
emptied.
-}
data Meta = Meta {drawnAt :: Maybe LocationId, moveEach :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RiggedToBlow = RiggedToBlow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riggedToBlow :: TreacheryCard RiggedToBlow
riggedToBlow = treacheryWith RiggedToBlow Cards.riggedToBlow (setMeta $ Meta Nothing False)

{- | "Peril. You must decide (choose one): Test [agility] (2). If you succeed,
you may move one investigator at your location to a connecting location. / Test
[agility] (4). If you succeed, you may move each investigator at your location
to a connecting location. Then, each investigator at the location where this card
was drawn takes 2 damage."
-}
instance RunMessage RiggedToBlow where
  runMessage msg t@(RiggedToBlow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      lid <- getLocationOf iid
      sid <- getRandom
      chooseOneM iid $ withI18n do
        chooseTest #agility 2 do
          push $ HandleAbilityOption iid (toSource attrs) 1
          revelationSkillTest sid iid attrs #agility (Fixed 2)
        chooseTest #agility 4 do
          push $ HandleAbilityOption iid (toSource attrs) 2
          revelationSkillTest sid iid attrs #agility (Fixed 4)
      doStep 1 msg
      pure $ RiggedToBlow $ setMeta (Meta lid False) attrs
    HandleAbilityOption _ (isSource attrs -> True) n -> do
      let m = toResult @Meta attrs.meta
      pure $ RiggedToBlow $ setMeta (m {moveEach = n == 2}) attrs
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      here <- select $ investigatorAt (locationWithInvestigator iid)
      connecting <- select $ ConnectedTo ForMovement (locationWithInvestigator iid)
      unless (null here || null connecting) do
        chooseOneM iid $ campaignI18n do
          if moveEach (toResult @Meta attrs.meta)
            then labeled "riggedToBlow.moveEach" do
              for_ here \i -> chooseTargetM iid connecting (moveTo attrs i)
            else labeled "riggedToBlow.moveOne" do
              chooseTargetM iid here \i -> chooseTargetM iid connecting (moveTo attrs i)
          unscoped $ labeled "doNotMoveToConnecting" nothing
      pure t
    DoStep 1 (Revelation _ (isSource attrs -> True)) -> do
      for_ (drawnAt $ toResult @Meta attrs.meta) \lid ->
        selectEach (investigatorAt $ LocationWithId lid) \i -> assignDamage i attrs 2
      pure t
    _ -> RiggedToBlow <$> liftRunMessage msg attrs
