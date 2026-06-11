module Arkham.Treachery.Cards.DreadfulMechanism (dreadfulMechanism) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SlotType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (PerformAction)

newtype Meta = Meta {usedMechanism :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DreadfulMechanism = DreadfulMechanism (TreacheryAttrs `With` Meta)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadfulMechanism :: TreacheryCard DreadfulMechanism
dreadfulMechanism = treachery (DreadfulMechanism . (`with` Meta False)) Cards.dreadfulMechanism

instance HasAbilities DreadfulMechanism where
  getAbilities (DreadfulMechanism (With a _)) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ PerformAction #after You AnyAction
    , skillTestAbility $ restricted a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage DreadfulMechanism where
  runMessage msg t@(DreadfulMechanism (With attrs meta)) = runQueueT $ case msg of
    -- The forced effect punishes any action other than this card's own
    -- [action] ability; that activation is tracked via the meta flag.
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      if usedMechanism meta
        then pure $ DreadfulMechanism $ attrs `with` Meta False
        else do
          assignDamage iid attrs 1
          assets <- select $ AssetControlledBy (InvestigatorWithId iid) <> AssetInSlot HandSlot
          chooseOrRunOneM iid do
            targets assets (toDiscardBy iid attrs)
          toDiscardBy iid attrs attrs
          pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 2)
      pure $ DreadfulMechanism $ attrs `with` Meta True
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    _ -> DreadfulMechanism . (`with` meta) <$> liftRunMessage msg attrs
