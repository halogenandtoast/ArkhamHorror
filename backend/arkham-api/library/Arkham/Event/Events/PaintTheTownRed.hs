module Arkham.Event.Events.PaintTheTownRed (paintTheTownRed) where

import Arkham.Enemy
import Arkham.Enemy.Types.Attrs
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Calculation
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Elite))

newtype PaintTheTownRed = PaintTheTownRed EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paintTheTownRed :: EventCard PaintTheTownRed
paintTheTownRed = event PaintTheTownRed Cards.paintTheTownRed

instance RunMessage PaintTheTownRed where
  runMessage msg e@(PaintTheTownRed attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search
        iid
        attrs
        EncounterDeckTarget
        [fromTopOfDeck 9]
        (basic $ #enemy <> not_ (withTrait Elite))
        (defer attrs IsDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) deck cards -> do
      chooseTargetM iid cards \card -> do
        drawCardFrom iid deck card
        handleTarget iid attrs card
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      card <- fetchCard cid
      eid <- getRandom
      for_ (attr enemyHealth (lookupEnemy card.cardCode eid card.id))
        $ calculate
        >=> gainResources iid attrs
      pure e
    _ -> PaintTheTownRed <$> liftRunMessage msg attrs
