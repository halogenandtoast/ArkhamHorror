module Arkham.Treachery.Cards.ThePitBelow
  ( thePitBelow
  , ThePitBelow(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Investigator.Attrs ( Field(..))
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner
import Arkham.Treachery.Helpers

newtype ThePitBelow = ThePitBelow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePitBelow :: TreacheryCard ThePitBelow
thePitBelow = treachery ThePitBelow Cards.thePitBelow

instance HasModifiersFor env ThePitBelow where
  getModifiersFor _ (LocationTarget lid) (ThePitBelow attrs) =
    pure
      $ toModifiers attrs [ ShroudModifier 1 | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage env ThePitBelow where
  runMessage msg t@(ThePitBelow attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      case mlid of
        Nothing -> push (Discard $ toTarget attrs)
        Just lid -> do
          hasThePitBelow <- selectAny $ TreacheryAt (LocationWithId lid) <> treacheryIs Cards.thePitBelow
          if hasThePitBelow
             then pushAll [Discard (toTarget attrs), Surge iid (toSource attrs)]
             else push (AttachTreachery (toId attrs) $ LocationTarget lid)
      pure t
    _ -> ThePitBelow <$> runMessage msg attrs
