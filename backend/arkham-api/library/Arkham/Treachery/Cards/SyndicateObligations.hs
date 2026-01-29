module Arkham.Treachery.Cards.SyndicateObligations (syndicateObligations) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SyndicateObligations = SyndicateObligations TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

syndicateObligations :: TreacheryCard SyndicateObligations
syndicateObligations = treachery SyndicateObligations Cards.syndicateObligations

instance HasAbilities SyndicateObligations where
  getAbilities (SyndicateObligations a) =
    [ restricted a 1 InYourThreatArea $ forced $ SpendsResources #after You AnyValue
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage SyndicateObligations where
  runMessage msg t@(SyndicateObligations attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> SyndicateObligations <$> liftRunMessage msg attrs
