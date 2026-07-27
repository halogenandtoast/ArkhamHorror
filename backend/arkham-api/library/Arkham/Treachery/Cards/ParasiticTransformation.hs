module Arkham.Treachery.Cards.ParasiticTransformation (parasiticTransformation) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ParasiticTransformation = ParasiticTransformation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parasiticTransformation :: TreacheryCard ParasiticTransformation
parasiticTransformation = treachery ParasiticTransformation Cards.parasiticTransformation

instance HasAbilities ParasiticTransformation where
  getAbilities (ParasiticTransformation a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ EnemyEngaged #after You AnyEnemy
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage ParasiticTransformation where
  runMessage msg t@(ParasiticTransformation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ParasiticTransformation <$> liftRunMessage msg attrs
