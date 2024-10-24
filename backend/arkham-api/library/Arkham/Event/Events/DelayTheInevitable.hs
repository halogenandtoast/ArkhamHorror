module Arkham.Event.Events.DelayTheInevitable (delayTheInevitable, DelayTheInevitable (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DelayTheInevitable = DelayTheInevitable EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delayTheInevitable :: EventCard DelayTheInevitable
delayTheInevitable = event DelayTheInevitable Cards.delayTheInevitable

instance HasAbilities DelayTheInevitable where
  getAbilities (DelayTheInevitable a) =
    [ restricted a 1 ControlsThis $ freeReaction (DealtDamageOrHorror #when AnySource You)
    , restricted a 2 ControlsThis $ forced $ PhaseEnds #when #mythos
    ]

getDamageAndHorror :: [Window] -> (Int, Int)
getDamageAndHorror [] = error "wrong window"
getDamageAndHorror ((windowType -> Window.WouldTakeDamageOrHorror _ _ damage horror) : _) =
  (damage, horror)
getDamageAndHorror (_ : xs) = getDamageAndHorror xs

instance RunMessage DelayTheInevitable where
  runMessage msg e@(DelayTheInevitable attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      iids <- select $ affectsOthers $ colocatedWith iid
      chooseOrRunOne
        iid
        [ targetLabel investigator [PlaceEvent eid $ InPlayArea investigator]
        | investigator <- iids
        ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 (getDamageAndHorror -> (damage, horror)) _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      chooseOrRunOneM iid do
        when (damage > 0) $ labeled "Cancel Damage" $ push $ CancelDamage iid damage
        when (horror > 0) $ labeled "Cancel Horror" $ push $ CancelHorror iid horror
        when (damage > 0 && horror > 0) do
          labeled "Cancel Horror and Damage" $ pushAll [CancelDamage iid damage, CancelHorror iid horror]
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      canAfford <- fieldMap InvestigatorResources (> 2) iid
      chooseOrRunOneM iid do
        when canAfford do
          labeled "Spend 2 Resources" $ push $ SpendResources iid 2
        labeled "Discard Delay the Inevitable" $ toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> DelayTheInevitable <$> liftRunMessage msg attrs
