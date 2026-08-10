module Arkham.Treachery.Cards.WorldsMerge (worldsMerge) where

import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (AgendaSequence))
import Arkham.Classes.HasGame
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WorldsMerge = WorldsMerge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

worldsMerge :: TreacheryCard WorldsMerge
worldsMerge = treachery WorldsMerge Cards.worldsMerge

getStep :: HasGame m => Maybe AgendaId -> m Int
getStep Nothing = pure 3 -- if no agenda than act is 3
getStep (Just agenda) = do
  side <- fieldMap AgendaSequence AS.agendaStep agenda
  case side of
    AS.AgendaStep step -> pure step

instance RunMessage WorldsMerge where
  runMessage msg t@(WorldsMerge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      n <- getStep =<< selectOne (AgendaWithSide AS.C)
      assignHorror iid attrs n
      chooseAndDiscardCards iid attrs n
      pure t
    _ -> WorldsMerge <$> liftRunMessage msg attrs
