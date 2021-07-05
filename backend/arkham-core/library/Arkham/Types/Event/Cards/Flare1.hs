module Arkham.Types.Event.Cards.Flare1 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards (flare1)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype Flare1 = Flare1 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flare1 :: EventCard Flare1
flare1 = event Flare1 Cards.flare1

instance HasModifiersFor env Flare1 where
  getModifiersFor = noModifiersFor

instance HasActions env ActionType => HasActions env Flare1 where
  getActions i (InHandWindow ownerId NonFast) (Flare1 attrs) | i == ownerId = do
    canFight <- hasFightActions i NonFast
    pure
      $ [ ActivateCardAbilityAction
            i
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility (Just Action.Fight) (ActionCost 1))
            )
        | canFight
        ]
      <> [ ActivateCardAbilityAction
             i
             (mkAbility
               (toSource attrs)
               2
               (ActionAbility Nothing (ActionCost 1))
             )
         ]
  getActions i window (Flare1 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Flare1 where
  runMessage msg e@(Flare1 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == eventId ->
      e <$ unshiftMessages [Discard (EventTarget eid)]
    _ -> Flare1 <$> runMessage msg attrs
