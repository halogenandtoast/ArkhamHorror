module Arkham.Types.Treachery.Cards.MaskedHorrors where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MaskedHorrors = MaskedHorrors TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedHorrors :: TreacheryCard MaskedHorrors
maskedHorrors = treachery MaskedHorrors Cards.maskedHorrors

instance HasModifiersFor env MaskedHorrors where
  getModifiersFor = noModifiersFor

instance HasActions env MaskedHorrors where
  getActions i window (MaskedHorrors attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env MaskedHorrors where
  runMessage msg t@(MaskedHorrors attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      iids <- getInvestigatorIds
      targetInvestigators <- map fst . filter ((>= 2) . snd) <$> for
        iids
        (\iid -> do
          clueCount <- unClueCount <$> getCount iid
          pure (iid, clueCount)
        )
      t <$ if null targetInvestigators
        then unshiftMessages
          ([ InvestigatorAssignDamage iid source DamageAny 0 2
           | iid <- targetInvestigators
           ]
          <> [Discard (TreacheryTarget treacheryId)]
          )
        else unshiftMessages
          [ PlaceDoomOnAgenda
          , AdvanceAgendaIfThresholdSatisfied
          , Discard (TreacheryTarget treacheryId)
          ]
    _ -> MaskedHorrors <$> runMessage msg attrs
