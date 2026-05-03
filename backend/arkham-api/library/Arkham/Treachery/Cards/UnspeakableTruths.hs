module Arkham.Treachery.Cards.UnspeakableTruths (unspeakableTruths) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues)

newtype UnspeakableTruths = UnspeakableTruths TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unspeakableTruths :: TreacheryCard UnspeakableTruths
unspeakableTruths = treachery UnspeakableTruths Cards.unspeakableTruths

instance HasAbilities UnspeakableTruths where
  getAbilities (UnspeakableTruths a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ DiscoverClues #after You Anywhere (atLeast 1)
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage UnspeakableTruths where
  runMessage msg t@(UnspeakableTruths attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      alreadyHasOne <- selectAny $ treacheryInThreatAreaOf iid <> treacheryIs Cards.unspeakableTruths
      if alreadyHasOne
        then toDiscardBy iid (toSource attrs) attrs
        else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> UnspeakableTruths <$> liftRunMessage msg attrs
