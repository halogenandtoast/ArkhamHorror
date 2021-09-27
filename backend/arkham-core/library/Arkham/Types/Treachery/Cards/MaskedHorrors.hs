module Arkham.Types.Treachery.Cards.MaskedHorrors where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MaskedHorrors = MaskedHorrors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedHorrors :: TreacheryCard MaskedHorrors
maskedHorrors = treachery MaskedHorrors Cards.maskedHorrors

instance TreacheryRunner env => RunMessage env MaskedHorrors where
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
