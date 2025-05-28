module Arkham.Treachery.Cards.ResentfulWilds (resentfulWilds) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ResentfulWilds = ResentfulWilds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

resentfulWilds :: TreacheryCard ResentfulWilds
resentfulWilds = treachery ResentfulWilds Cards.resentfulWilds

instance HasAbilities ResentfulWilds where
  getAbilities (ResentfulWilds a) = case a.placement of
    AttachedToLocation lid -> [mkAbility a 1 $ forced $ Explored #after You (be lid) #success]
    _ -> []

instance RunMessage ResentfulWilds where
  runMessage msg t@(ResentfulWilds attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.resentfulWilds)
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure t
    _ -> ResentfulWilds <$> liftRunMessage msg attrs
