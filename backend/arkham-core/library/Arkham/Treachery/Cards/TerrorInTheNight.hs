module Arkham.Treachery.Cards.TerrorInTheNight (terrorInTheNight, TerrorInTheNight (..)) where

import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorInTheNight = TerrorInTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorInTheNight :: TreacheryCard TerrorInTheNight
terrorInTheNight = treachery TerrorInTheNight Cards.terrorInTheNight

instance RunMessage TerrorInTheNight where
  runMessage msg t@(TerrorInTheNight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      revelationSkillTest iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      aid <- selectJust AnyAgenda
      other <- select $ treacheryIs Cards.terrorInTheNight
      iids <- getInvestigatorIds
      attached <- filterByField TreacheryPlacement (== AttachedToAgenda aid) other
      attachTreachery attrs aid
      when (n >= 3) $ gainSurge attrs
      when (length attached >= 2) do
        toDiscard attrs attrs
        for_ attached $ toDiscard attrs
        for_ iids \iid -> assignHorror iid attrs 3
      pure t
    _ -> TerrorInTheNight <$> liftRunMessage msg attrs
