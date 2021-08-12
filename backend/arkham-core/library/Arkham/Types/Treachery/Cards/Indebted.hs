module Arkham.Types.Treachery.Cards.Indebted
  ( Indebted(..)
  , indebted
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype Indebted = Indebted TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indebted :: TreacheryCard Indebted
indebted = treachery Indebted Cards.indebted

instance HasModifiersFor env Indebted where
  getModifiersFor _ (InvestigatorTarget iid) (Indebted attrs) =
    pure $ toModifiers
      attrs
      [ StartingResources (-2) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Indebted where
  runMessage msg t@(Indebted attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    _ -> Indebted <$> runMessage msg attrs
