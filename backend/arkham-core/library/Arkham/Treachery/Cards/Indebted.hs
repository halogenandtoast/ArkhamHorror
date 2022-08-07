module Arkham.Treachery.Cards.Indebted
  ( Indebted(..)
  , indebted
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Treachery.Runner
import Arkham.Treachery.Helpers

newtype Indebted = Indebted TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indebted :: TreacheryCard Indebted
indebted = treachery Indebted Cards.indebted

instance HasModifiersFor Indebted where
  getModifiersFor (InvestigatorTarget iid) (Indebted attrs) =
    pure $ toModifiers
      attrs
      [ StartingResources (-2) | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ = pure []

instance RunMessage Indebted where
  runMessage msg t@(Indebted attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    _ -> Indebted <$> runMessage msg attrs
