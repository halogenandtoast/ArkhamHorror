module Arkham.Treachery.Cards.TerrorInTheNight (terrorInTheNight) where

import Arkham.Matcher
import Arkham.Message.Lifted.Placement
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
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      attached <- select $ treacheryIs Cards.terrorInTheNight <> TreacheryWithPlacement NextToAgenda
      when (n >= 3) $ gainSurge attrs
      if length attached >= 2
        then do
          for_ attached $ toDiscard attrs
          eachInvestigator \iid -> assignHorror iid attrs 3
        else place attrs NextToAgenda
      pure t
    _ -> TerrorInTheNight <$> liftRunMessage msg attrs
