module Arkham.Location.Cards.Galata (galata) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype Galata = Galata LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galata :: LocationCard Galata
galata = symbolLabel $ location Galata Cards.galata 3 (PerPlayer 1)

instance HasAbilities Galata where
  getAbilities (Galata a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists ConcealedCardAny)
      $ FastAbility' (ResourceCost 3) [#parley]

instance RunMessage Galata where
  runMessage msg l@(Galata attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- selectMap (.id) ConcealedCardAny
      chooseTargetM iid concealed $ exposeConcealed iid (attrs.ability 1)
      pure l
    _ -> Galata <$> liftRunMessage msg attrs
