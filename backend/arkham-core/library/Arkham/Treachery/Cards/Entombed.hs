module Arkham.Treachery.Cards.Entombed
  ( entombed
  , Entombed(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Runner

newtype Entombed = Entombed TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entombed :: TreacheryCard Entombed
entombed = treachery Entombed Cards.entombed

instance HasModifiersFor Entombed where
  getModifiersFor (InvestigatorTarget iid) (Entombed attrs) =
    if treacheryOnInvestigator iid attrs
       then
        pure $ toModifiers
          attrs
          [ CannotMove, CannotDisengageEnemies ]
       else pure []
  getModifiersFor _ _ = pure []

instance RunMessage Entombed where
  runMessage msg t@(Entombed attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ AttachTreachery (toId attrs) (idToTarget iid)
      pure t
    _ -> Entombed <$> runMessage msg attrs
