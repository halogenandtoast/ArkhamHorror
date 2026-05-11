module Arkham.Treachery.Cards.MarkOfElokoss (markOfElokoss) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MarkOfElokoss = MarkOfElokoss TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markOfElokoss :: TreacheryCard MarkOfElokoss
markOfElokoss = treachery MarkOfElokoss Cards.markOfElokoss

instance HasAbilities MarkOfElokoss where
  getAbilities (MarkOfElokoss a) =
    [ restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage MarkOfElokoss where
  runMessage msg t@(MarkOfElokoss attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageWithStrategy iid (attrs.ability 1) (DamageAssetsFirst AnyAsset) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> MarkOfElokoss <$> liftRunMessage msg attrs
