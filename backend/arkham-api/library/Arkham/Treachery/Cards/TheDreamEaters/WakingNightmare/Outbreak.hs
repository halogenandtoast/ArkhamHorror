module Arkham.Treachery.Cards.TheDreamEaters.WakingNightmare.Outbreak (outbreak, Outbreak (..)) where

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheDreamEaters.WakingNightmare.Helpers
import Arkham.Story.CardDefs.TheDreamEaters.WakingNightmare qualified as Stories
import Arkham.Treachery.CardDefs.TheDreamEaters.WakingNightmare qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Outbreak = Outbreak TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outbreak :: TreacheryCard Outbreak
outbreak = treachery Outbreak Cards.outbreak

instance HasModifiersFor Outbreak where
  getModifiersFor (Outbreak attrs) = do
    atInfested <- attrs.drawnBy <=~> InvestigatorAt InfestedLocation
    modifiedWhen_
      attrs
      atInfested
      (StoryTarget $ StoryId $ Stories.theInfestationBegins.cardCode)
      [MetaModifier $ object ["treatTabletAsSkill" .= True]]

instance RunMessage Outbreak where
  runMessage msg t@(Outbreak attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      makeInfestationTest
      pure t
    _ -> Outbreak <$> liftRunMessage msg attrs
