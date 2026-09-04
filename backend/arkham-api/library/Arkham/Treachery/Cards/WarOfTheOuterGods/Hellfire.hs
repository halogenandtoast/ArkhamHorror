module Arkham.Treachery.Cards.WarOfTheOuterGods.Hellfire (hellfire) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Treachery.CardDefs.WarOfTheOuterGods qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Hellfire = Hellfire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hellfire :: TreacheryCard Hellfire
hellfire = treachery Hellfire Cards.hellfire

instance RunMessage Hellfire where
  runMessage msg t@(Hellfire attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      clues <- field InvestigatorClues iid
      if clues >= 4
        then do
          assignDamage iid attrs 2
          push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 2
        else scenarioI18n $ chooseOrRunOneM iid do
          labeled "take2Damage" $ assignDamage iid attrs 2
          when (clues > 0) do
            labeled "placeCluesOnYourLocation" $ push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 2
      pure t
    _ -> Hellfire <$> liftRunMessage msg attrs
