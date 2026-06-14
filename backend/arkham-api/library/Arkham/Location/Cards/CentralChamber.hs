module Arkham.Location.Cards.CentralChamber (centralChamber) where

import Arkham.Ability
import Arkham.Classes.HasGame (HasGame)
import Arkham.Direction (GridDirection)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid (updatePosition)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier (UIModifier (..))
import Arkham.Scenarios.TheApiary.Helpers
import Arkham.Tracing (Tracing)

newtype CentralChamber = CentralChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centralChamber :: LocationCard CentralChamber
centralChamber = location CentralChamber Cards.centralChamber 4 (Static 3)

-- | The Central Chamber sits in the centre of a 4-location ring and is only
-- connected to the ring location it currently "faces" (the location beneath its
-- bottom edge). The facing lives in the scenario meta and is rotated by the act.
-- We translate the facing into a grid offset (via 'updatePosition', the same
-- function the engine uses) to find the faced location, then emit a bidirectional
-- connection modifier between it and the Central Chamber. We also emit a UI
-- rotation so the card visually points at the location it faces.
getFacedLocation :: (HasGame m, Tracing m) => LocationAttrs -> GridDirection -> m (Maybe LocationId)
getFacedLocation attrs facing =
  case locationPosition attrs of
    Nothing -> pure Nothing
    Just pos -> selectOne $ LocationInPosition (updatePosition pos facing)

instance HasModifiersFor CentralChamber where
  getModifiersFor (CentralChamber a) = do
    facing <- getCentralChamberFacing
    modifySelf a [UIModifier $ Rotated (facingDegrees facing)]
    whenRevealed a do
      getFacedLocation a facing >>= \case
        Nothing -> pure ()
        Just faced -> do
          modifySelf a [ConnectedToWhen (be a) (LocationWithId faced)]
          modifySelect a (LocationWithId faced) [ConnectedToWhen (LocationWithId faced) (be a)]

instance HasAbilities CentralChamber where
  getAbilities (CentralChamber a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #after You

instance RunMessage CentralChamber where
  runMessage msg l@(CentralChamber attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 1)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      connecting <- select $ ConnectedTo ForMovement (be attrs)
      mMother <- selectOne $ enemyIs Enemies.mother
      chooseOneM iid do
        unless (null connecting) do
          labeled' "moveToConnecting"
            $ chooseTargetM iid connecting \lid -> moveTo (attrs.ability 1) iid lid
        for_ mMother \mother ->
          labeled' "motherAttacks" $ initiateEnemyAttack mother (attrs.ability 1) iid
      pure l
    _ -> CentralChamber <$> liftRunMessage msg attrs
