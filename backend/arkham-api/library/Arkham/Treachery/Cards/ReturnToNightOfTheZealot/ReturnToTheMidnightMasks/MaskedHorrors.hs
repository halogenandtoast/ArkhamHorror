module Arkham.Treachery.Cards.ReturnToNightOfTheZealot.ReturnToTheMidnightMasks.MaskedHorrors (maskedHorrors) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ReturnToNightOfTheZealot.ReturnToTheMidnightMasks qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MaskedHorrors = MaskedHorrors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedHorrors :: TreacheryCard MaskedHorrors
maskedHorrors = treachery MaskedHorrors Cards.maskedHorrors

instance RunMessage MaskedHorrors where
  runMessage msg t@(MaskedHorrors attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      investigators <-
        map fst
          . filter ((>= 2) . snd)
          <$> selectWithField InvestigatorClues UneliminatedInvestigator
      if null investigators
        then placeDoomOnAgendaAndCheckAdvance 1
        else for_ investigators \iid -> assignHorror iid attrs 2
      pure t
    _ -> MaskedHorrors <$> liftRunMessage msg attrs
