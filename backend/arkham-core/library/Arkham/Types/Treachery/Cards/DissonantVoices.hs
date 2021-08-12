module Arkham.Types.Treachery.Cards.DissonantVoices where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype DissonantVoices= DissonantVoices TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissonantVoices :: TreacheryCard DissonantVoices
dissonantVoices = treachery DissonantVoices Cards.dissonantVoices

instance HasModifiersFor env DissonantVoices where
  getModifiersFor _ (InvestigatorTarget iid) (DissonantVoices attrs) =
    pure $ toModifiers
      attrs
      [ CannotPlay [(AssetType, mempty), (EventType, mempty)]
      | treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env DissonantVoices where
  runMessage msg t@(DissonantVoices attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId (InvestigatorTarget iid))
    EndRound -> t <$ push (Discard $ toTarget attrs)
    _ -> DissonantVoices <$> runMessage msg attrs
