module Arkham.Treachery.Cards.Outbreak (outbreak, Outbreak (..)) where

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Message
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Outbreak = Outbreak TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outbreak :: TreacheryCard Outbreak
outbreak = treachery Outbreak Cards.outbreak

instance HasModifiersFor Outbreak where
  getModifiersFor (Outbreak attrs) = do
    modified_
      attrs
      (StoryTarget $ StoryId $ Stories.theInfestationBegins.cardCode)
      [MetaModifier $ object ["treatTabletAsSkill" .= True]]

instance RunMessage Outbreak where
  runMessage msg t@(Outbreak attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      makeInfestationTest
      pure t
    _ -> Outbreak <$> liftRunMessage msg attrs
