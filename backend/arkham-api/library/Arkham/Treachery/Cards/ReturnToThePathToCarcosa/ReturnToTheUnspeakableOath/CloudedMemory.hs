module Arkham.Treachery.Cards.ReturnToThePathToCarcosa.ReturnToTheUnspeakableOath.CloudedMemory (cloudedMemory) where

import Arkham.Helpers.Log
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThePathToCarcosa.TheUnspeakableOath.Helpers
import Arkham.Treachery.CardDefs.ReturnToThePathToCarcosa.ReturnToTheUnspeakableOath qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CloudedMemory = CloudedMemory TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloudedMemory :: TreacheryCard CloudedMemory
cloudedMemory = treachery CloudedMemory Cards.cloudedMemory

instance RunMessage CloudedMemory where
  runMessage msg t@(CloudedMemory attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> scenarioI18n do
      allRemembered <-
        filterM
          remembered
          [ SetAFireInTheKitchen
          , IncitedAFightAmongstThePatients
          , DistractedTheGuards
          , ReleasedADangerousPatient
          , KnowTheGuardsPatrols
          , RecalledTheWayOut
          , YouTookTheKeysByForce
          ]

      let
        toKey = \case
          SetAFireInTheKitchen -> "setAFireInTheKitchen"
          IncitedAFightAmongstThePatients -> "incitedAFightAmongstThePatients"
          DistractedTheGuards -> "distractedTheGuards"
          ReleasedADangerousPatient -> "releasedADangerousPatient"
          KnowTheGuardsPatrols -> "knowTheGuardsPatrols"
          RecalledTheWayOut -> "recalledTheWayOut"
          YouTookTheKeysByForce -> "youTookTheKeysByForce"
          _ -> error "Unknown key"
      chooseOneM iid do
        when (notNull allRemembered) do
          labeled "cloudedMemory.forget" do
            chooseOneM iid do
              questionLabeled "cloudedMemory.chooseForget"
              for_ allRemembered \r -> do
                labeled ("remembered." <> toKey r) (forget r)
        labeled "cloudedMemory.horror" do
          assignHorror iid attrs 1
          gainSurge attrs
      pure t
    _ -> CloudedMemory <$> liftRunMessage msg attrs
