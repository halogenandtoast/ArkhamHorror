module Arkham.Treachery.Cards.Obsessive (obsessive, Obsessive (..)) where

import Arkham.Ability
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Obsessive = Obsessive TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsessive :: TreacheryCard Obsessive
obsessive = treachery Obsessive Cards.obsessive

instance HasAbilities Obsessive where
  getAbilities (Obsessive a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ forced $ TurnBegins #when You
    , restrictedAbility a 2 (InThreatAreaOf $ InvestigatorAt YourLocation)
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage Obsessive where
  runMessage msg t@(Obsessive attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inHandOf iid <> basic (NonWeakness <> CardWithoutKeyword Keyword.Hidden)
      for_ (nonEmpty cards) $ \cards' -> do
        card <- sample cards'
        push $ DiscardCard iid (attrs.ability 1) card.id
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Obsessive <$> liftRunMessage msg attrs
