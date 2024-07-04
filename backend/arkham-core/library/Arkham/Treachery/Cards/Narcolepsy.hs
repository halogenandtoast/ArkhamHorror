module Arkham.Treachery.Cards.Narcolepsy (narcolepsy, Narcolepsy (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Narcolepsy = Narcolepsy TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narcolepsy :: TreacheryCard Narcolepsy
narcolepsy = treachery Narcolepsy Cards.narcolepsy

instance HasModifiersFor Narcolepsy where
  getModifiersFor (InvestigatorTarget iid) (Narcolepsy attrs) | treacheryInThreatArea iid attrs = do
    modified
      attrs
      [CannotTakeAction IsAnyAction, CannotTriggerAbilityMatching AnyAbility, CannotPlay AnyCard]
  getModifiersFor _ _ = pure []

instance HasAbilities Narcolepsy where
  getAbilities (Narcolepsy a) =
    [ restrictedAbility a 1 OnSameLocation actionAbility
    , restrictedAbility a 2 (InThreatAreaOf You)
        $ forced
        $ oneOf [InvestigatorTakeDamage #after You AnySource, InvestigatorTakeHorror #after You AnySource]
    ]

instance RunMessage Narcolepsy where
  runMessage msg t@(Narcolepsy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Narcolepsy <$> liftRunMessage msg attrs
