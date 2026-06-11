module Arkham.Treachery.Cards.HellfireWarOfTheOuterGods (hellfireWarOfTheOuterGods) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HellfireWarOfTheOuterGods = HellfireWarOfTheOuterGods TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hellfireWarOfTheOuterGods :: TreacheryCard HellfireWarOfTheOuterGods
hellfireWarOfTheOuterGods = treachery HellfireWarOfTheOuterGods Cards.hellfireWarOfTheOuterGods

instance RunMessage HellfireWarOfTheOuterGods where
  runMessage msg t@(HellfireWarOfTheOuterGods attrs) = runQueueT $ case msg of
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
          labeled' "take2Damage" $ assignDamage iid attrs 2
          when (clues > 0) do
            labeled' "placeCluesOnYourLocation" $ push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 2
      pure t
    _ -> HellfireWarOfTheOuterGods <$> liftRunMessage msg attrs
