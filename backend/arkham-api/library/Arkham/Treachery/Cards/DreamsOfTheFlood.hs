module Arkham.Treachery.Cards.DreamsOfTheFlood (dreamsOfTheFlood) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues)

newtype DreamsOfTheFlood = DreamsOfTheFlood TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfTheFlood :: TreacheryCard DreamsOfTheFlood
dreamsOfTheFlood = treachery DreamsOfTheFlood Cards.dreamsOfTheFlood

instance HasAbilities DreamsOfTheFlood where
  getAbilities (DreamsOfTheFlood a) =
    [ restricted a 1 (InYourThreatArea <> youExist (not_ $ DiscoveredCluesThis #turn))
        $ forced (DiscoverClues #when You Anywhere (atLeast 1))
    ]

instance RunMessage DreamsOfTheFlood where
  runMessage msg t@(DreamsOfTheFlood attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      shuffleIntoDeck iid attrs
      pure t
    _ -> DreamsOfTheFlood <$> liftRunMessage msg attrs
