module Arkham.Act.Cards.PedalToTheMetal (pedalToTheMetal) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Types (Field (AssetDriver))
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (AddKeyword, VehicleCannotMove), modifySelect)
import Arkham.Helpers.Vehicle (moveVehicle)
import Arkham.Keyword (Keyword (Hunter))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Road))

newtype PedalToTheMetal = PedalToTheMetal ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pedalToTheMetal :: ActCard PedalToTheMetal
pedalToTheMetal = act (1, A) PedalToTheMetal Cards.pedalToTheMetal Nothing

instance HasModifiersFor PedalToTheMetal where
  getModifiersFor (PedalToTheMetal a) = modifySelect a AnyEnemy [AddKeyword Hunter]

instance HasAbilities PedalToTheMetal where
  getAbilities (PedalToTheMetal x) =
    extend1 x
      $ restricted
        x
        1
        (exists $ #vehicle <> AssetWithSubtitle "Running" <> AssetWithoutModifier VehicleCannotMove)
      $ forced
      $ PhaseEnds #when #investigation

instance RunMessage PedalToTheMetal where
  runMessage msg a@(PedalToTheMetal attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      vehicles <-
        filterM (fieldMap AssetDriver isJust)
          =<< select (#vehicle <> AssetWithSubtitle "Running" <> AssetWithoutModifier VehicleCannotMove)
      lead <- getLead
      chooseOneAtATimeM lead $ targets vehicles $ handleTarget lead (attrs.ability 1)
      pure a
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      getLocationOf aid >>= traverse_ \lid -> do
        driver <- fieldJust AssetDriver aid
        roads <- select $ withTrait Road <> connectedTo (LocationWithId lid)
        chooseTargetM driver roads $ moveVehicle aid lid
      pure a
    _ -> PedalToTheMetal <$> liftRunMessage msg attrs
