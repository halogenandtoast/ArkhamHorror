module Arkham.Treachery.Cards.NebulousMiasma (nebulousMiasma) where

import Arkham.Ability
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NebulousMiasma = NebulousMiasma TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nebulousMiasma :: TreacheryCard NebulousMiasma
nebulousMiasma = treachery NebulousMiasma Cards.nebulousMiasma

instance HasAbilities NebulousMiasma where
  getAbilities (NebulousMiasma a) =
    [ restricted a 1 OnSameLocation $ forced $ TurnEnds #when You
    , mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage NebulousMiasma where
  runMessage msg t@(NebulousMiasma attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directHorror iid (attrs.ability 1) 1
      selectEach (AssetWithSanity <> assetControlledBy iid) \asset ->
        push $ DealAssetDirectDamage asset (attrs.ability 1) 0 1
      pure t
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> NebulousMiasma <$> liftRunMessage msg attrs
