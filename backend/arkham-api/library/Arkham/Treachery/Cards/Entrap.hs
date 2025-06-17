module Arkham.Treachery.Cards.Entrap (entrap) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Guest))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Entrap = Entrap TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entrap :: TreacheryCard Entrap
entrap = treachery Entrap Cards.entrap

instance RunMessage Entrap where
  runMessage msg t@(Entrap attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectOne (enemyIs Enemies.theBloodlessMan) >>= \case
        Nothing -> gainSurge attrs
        Just eid -> withLocationOf eid \eloc -> do
          withLocationOf iid \iloc -> do
            if eloc == iloc
              then do
                guests <- select $ assetControlledBy iid <> AssetWithTrait Guest
                chooseTargetM iid guests \aid -> do
                  push $ ScenarioSpecific "spellbound" (toJSON aid)
              else do
                ready eid
                moveUntil eid iloc
                enemyEngageInvestigator eid iid
      pure t
    _ -> Entrap <$> liftRunMessage msg attrs
