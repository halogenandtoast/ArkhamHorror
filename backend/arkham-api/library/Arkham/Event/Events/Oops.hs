module Arkham.Event.Events.Oops (oops) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Window
import Arkham.Location.Types (Field (LocationConcealedCards))
import Arkham.Matcher
import Arkham.Projection (fieldMap)

newtype Oops = Oops EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops :: EventCard Oops
oops = event Oops Cards.oops

instance RunMessage Oops where
  runMessage msg e@(Oops attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let enemy = attackedEnemy attrs.windows
      enemies <- select $ enemyAtLocationWith iid <> not_ (be enemy)
      mconcealed <-
        runMaybeT $ MaybeT (getLocationOf iid) >>= MaybeT . fieldMap LocationConcealedCards headMay

      chooseOrRunOneM iid do
        targets enemies \x -> push $ InvestigatorDamageEnemy iid x (toSource enemy)
        for_ mconcealed \concealed -> targeting concealed $ doFlip iid attrs concealed
      pure e
    _ -> Oops <$> liftRunMessage msg attrs
