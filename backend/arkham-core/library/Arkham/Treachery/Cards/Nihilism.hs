module Arkham.Treachery.Cards.Nihilism (nihilism, Nihilism (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (RevealChaosToken)

newtype Nihilism = Nihilism TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nihilism :: TreacheryCard Nihilism
nihilism = treachery Nihilism Cards.nihilism

instance HasAbilities Nihilism where
  getAbilities (Nihilism a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ oneOf
          [ RevealChaosToken #after You #autofail
          , CancelChaosToken #after You #autofail
          , IgnoreChaosToken #after You #autofail
          ]
    , restrictedAbility a 2 OnSameLocation (ActionAbility [] $ ActionCost 2)
    ]

instance RunMessage Nihilism where
  runMessage msg t@(Nihilism attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Nihilism <$> liftRunMessage msg attrs
