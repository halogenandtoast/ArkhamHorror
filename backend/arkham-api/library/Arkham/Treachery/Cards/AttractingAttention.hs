module Arkham.Treachery.Cards.AttractingAttention (attractingAttention) where

import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AttractingAttention = AttractingAttention TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

attractingAttention :: TreacheryCard AttractingAttention
attractingAttention = treachery AttractingAttention Cards.attractingAttention

instance RunMessage AttractingAttention where
  runMessage msg t@(AttractingAttention attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        broodOfYogSothoth <- getBroodOfYogSothoth
        chooseOneAtATimeM iid do
          targets broodOfYogSothoth (`moveToward` LocationWithId lid)
      pure t
    _ -> AttractingAttention <$> liftRunMessage msg attrs
