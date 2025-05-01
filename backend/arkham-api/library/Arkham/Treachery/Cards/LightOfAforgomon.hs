module Arkham.Treachery.Cards.LightOfAforgomon (lightOfAforgomon) where

import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LightOfAforgomon = LightOfAforgomon TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightOfAforgomon :: TreacheryCard LightOfAforgomon
lightOfAforgomon = treachery LightOfAforgomon Cards.lightOfAforgomon

instance HasModifiersFor LightOfAforgomon where
  getModifiersFor (LightOfAforgomon attrs) = everyoneGets attrs [TreatAllDamageAsDirect]

instance RunMessage LightOfAforgomon where
  runMessage msg t@(LightOfAforgomon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      acts <- select $ NotAct $ ActWithTreachery $ treacheryIs Cards.lightOfAforgomon
      agendas <- select $ NotAgenda $ AgendaWithTreachery $ treacheryIs Cards.lightOfAforgomon
      chooseOneM iid do
        targets acts $ attachTreachery attrs
        targets agendas $ attachTreachery attrs
      pure t
    _ -> LightOfAforgomon <$> liftRunMessage msg attrs
