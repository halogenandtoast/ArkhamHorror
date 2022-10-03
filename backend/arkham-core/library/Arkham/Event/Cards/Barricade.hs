module Arkham.Event.Cards.Barricade
  ( barricade
  , Barricade(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Event.Helpers
import Arkham.Investigator.Types (Field(..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype Barricade = Barricade EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade :: EventCard Barricade
barricade = event Barricade Cards.barricade

instance HasModifiersFor Barricade where
  getModifiersFor (LocationTarget lid) (Barricade attrs) = pure $ toModifiers
    attrs
    [ CannotBeEnteredByNonElite
    | LocationTarget lid `elem` eventAttachedTarget attrs
    ]
  getModifiersFor _ _ = pure []

instance HasAbilities Barricade where
  getAbilities (Barricade x) = case eventAttachedTarget x of
    Just (LocationTarget lid) ->
      [ mkAbility x 1 $ ForcedAbility $ Leaves Timing.When You $ LocationWithId
          lid
      ]
    _ -> []

instance RunMessage Barricade where
  runMessage msg e@(Barricade attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- fieldMap InvestigatorLocation (fromJustNote "must be at a location") iid
      e <$ push (AttachEvent eid (LocationTarget lid))
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    _ -> Barricade <$> runMessage msg attrs
