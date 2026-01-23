module Arkham.Treachery.Cards.CaptivatingGleam (captivatingGleam) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CaptivatingGleam = CaptivatingGleam TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

captivatingGleam :: TreacheryCard CaptivatingGleam
captivatingGleam = treachery CaptivatingGleam Cards.captivatingGleam

instance HasAbilities CaptivatingGleam where
  getAbilities (CaptivatingGleam a) =
    [restricted a 1 (InThreatAreaOf You <> youExist (HandWith NoCards)) $ forced AnyWindow]

instance RunMessage CaptivatingGleam where
  runMessage msg t@(CaptivatingGleam attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      whenNone (treacheryInThreatAreaOf iid <> treacheryIs Cards.captivatingGleam) do
        placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 5
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> CaptivatingGleam <$> liftRunMessage msg attrs
