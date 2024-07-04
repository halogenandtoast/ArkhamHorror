module Arkham.Treachery.Cards.Chronophobia (chronophobia, Chronophobia (..)) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Chronophobia = Chronophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chronophobia :: TreacheryCard Chronophobia
chronophobia = treachery Chronophobia Cards.chronophobia

instance HasAbilities Chronophobia where
  getAbilities (Chronophobia x) =
    [ restrictedAbility x 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You
    , restrictedAbility x 2 OnSameLocation $ ActionAbility [] $ ActionCost 2
    ]

instance RunMessage Chronophobia where
  runMessage msg t@(Chronophobia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Chronophobia <$> liftRunMessage msg attrs
