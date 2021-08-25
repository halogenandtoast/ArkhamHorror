module Arkham.Types.Event.Cards.Barricade
  ( barricade
  , Barricade(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype Barricade = Barricade EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade :: EventCard Barricade
barricade = event Barricade Cards.barricade

instance HasModifiersFor env Barricade where
  getModifiersFor _ (LocationTarget lid) (Barricade attrs) = pure $ toModifiers
    attrs
    [ CannotBeEnteredByNonElite
    | LocationTarget lid `elem` eventAttachedTarget attrs
    ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env Barricade where
  getAbilities _ _ (Barricade x) = case eventAttachedTarget x of
    Just (LocationTarget lid) -> pure
      [ mkAbility x 1 $ ForcedAbility $ Leaves Timing.When You $ LocationWithId
          lid
      ]
    _ -> pure []

instance EventRunner env => RunMessage env Barricade where
  runMessage msg e@(Barricade attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      lid <- getId iid
      e <$ push (AttachEvent eid (LocationTarget lid))
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    _ -> Barricade <$> runMessage msg attrs
