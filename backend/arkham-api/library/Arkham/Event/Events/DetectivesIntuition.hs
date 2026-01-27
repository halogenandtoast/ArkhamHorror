module Arkham.Event.Events.DetectivesIntuition (detectivesIntuition) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)

newtype DetectivesIntuition = DetectivesIntuition EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectivesIntuition :: EventCard DetectivesIntuition
detectivesIntuition = event DetectivesIntuition Cards.detectivesIntuition

instance HasAbilities DetectivesIntuition where
  getAbilities (DetectivesIntuition a) =
    [ restricted a 1 (InYourHand <> DuringTurn (can.draw.cards You))
        $ triggered
          (DrawCard #after You (basic $ CardWithId a.cardId) AnyDeck)
          (RevealCost a.cardId)
    ]

instance RunMessage DetectivesIntuition where
  runMessage msg e@(DetectivesIntuition attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResources iid (attrs.ability 1) 2
      healDamage iid (attrs.ability 1) 1
      healHorror iid (attrs.ability 1) 1
      pure e
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      drawCards iid (attrs.ability 1) 2
      pure e
    _ -> DetectivesIntuition <$> liftRunMessage msg attrs
