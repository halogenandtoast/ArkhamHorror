module Arkham.Treachery.Cards.Wounded (wounded) where

import Arkham.Ability
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Wounded = Wounded TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wounded :: TreacheryCard Wounded
wounded = treachery Wounded Cards.wounded

instance HasAbilities Wounded where
  getAbilities (Wounded a) =
    [ restricted a 1 (InYourThreatArea <> DuringTurn You)
        $ forced
        $ Moves #when (You <> not_ InvestigatorThatMovedDuringTurn) AnySource Anywhere Anywhere
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage Wounded where
  runMessage msg t@(Wounded attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Wounded <$> liftRunMessage msg attrs
