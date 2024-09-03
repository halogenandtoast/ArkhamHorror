module Arkham.Treachery.Cards.MaskedHorrors where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MaskedHorrors = MaskedHorrors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedHorrors :: TreacheryCard MaskedHorrors
maskedHorrors = treachery MaskedHorrors Cards.maskedHorrors

instance RunMessage MaskedHorrors where
  runMessage msg t@(MaskedHorrors attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      iids <- getInvestigatorIds
      targetInvestigators <-
        map fst
          . filter ((>= 2) . snd)
          <$> for
            iids
            ( \iid -> do
                clueCount <- field InvestigatorClues iid
                pure (iid, clueCount)
            )
      if null targetInvestigators
        then push placeDoomOnAgendaAndCheckAdvance
        else
          pushAll
            [ InvestigatorAssignDamage iid source DamageAny 0 2
            | iid <- targetInvestigators
            ]
      pure t
    _ -> MaskedHorrors <$> runMessage msg attrs
