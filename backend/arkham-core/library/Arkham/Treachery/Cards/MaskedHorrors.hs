module Arkham.Treachery.Cards.MaskedHorrors where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Query
import Arkham.Treachery.Runner
import Arkham.Treachery.Runner

newtype MaskedHorrors = MaskedHorrors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedHorrors :: TreacheryCard MaskedHorrors
maskedHorrors = treachery MaskedHorrors Cards.maskedHorrors

instance TreacheryRunner env => RunMessage MaskedHorrors where
  runMessage msg t@(MaskedHorrors attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      iids <- getInvestigatorIds
      targetInvestigators <- map fst . filter ((>= 2) . snd) <$> for
        iids
        (\iid -> do
          clueCount <- unClueCount <$> getCount iid
          pure (iid, clueCount)
        )
      t <$ if null targetInvestigators
        then pushAll
          [ InvestigatorAssignDamage iid source DamageAny 0 2
          | iid <- targetInvestigators
          ]
        else pushAll [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
    _ -> MaskedHorrors <$> runMessage msg attrs
