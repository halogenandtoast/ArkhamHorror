module Arkham.Treachery.Cards.Yaztaroth (yaztaroth, Yaztaroth (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Yaztaroth = Yaztaroth TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yaztaroth :: TreacheryCard Yaztaroth
yaztaroth = treachery Yaztaroth Cards.yaztaroth

instance HasModifiersFor Yaztaroth where
  getModifiersFor (InvestigatorTarget iid) (Yaztaroth attrs) =
    pure $ toModifiers attrs $ do
      guard $ treacheryInThreatArea iid attrs
      [CannotPlay AssetCard, CannotPutIntoPlay AssetCard]
  getModifiersFor _ _ = pure []

instance HasAbilities Yaztaroth where
  getAbilities (Yaztaroth a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage Yaztaroth where
  runMessage msg t@(Yaztaroth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Yaztaroth <$> liftRunMessage msg attrs
