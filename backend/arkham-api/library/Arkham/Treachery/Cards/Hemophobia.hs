module Arkham.Treachery.Cards.Hemophobia (hemophobia) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Hemophobia = Hemophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hemophobia :: TreacheryCard Hemophobia
hemophobia = treachery Hemophobia Cards.hemophobia

instance HasAbilities Hemophobia where
  getAbilities (Hemophobia a) =
    [ playerLimit PerRound
        $ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ InvestigatorTakeDamage #after (at_ YourLocation) AnySource
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage Hemophobia where
  runMessage msg t@(Hemophobia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Hemophobia <$> liftRunMessage msg attrs
