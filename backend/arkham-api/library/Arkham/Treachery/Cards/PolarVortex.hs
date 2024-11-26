module Arkham.Treachery.Cards.PolarVortex (polarVortex, PolarVortex (..)) where

import Arkham.Ability
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Message (pattern DealAssetDamage)
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PolarVortex = PolarVortex TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

polarVortex :: TreacheryCard PolarVortex
polarVortex = treachery PolarVortex Cards.polarVortex

instance HasAbilities PolarVortex where
  getAbilities (PolarVortex a) =
    [ mkAbility a 1 $ forced $ TurnEnds #when (You <> at_ (locationWithTreachery a))
    , mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage PolarVortex where
  runMessage msg t@(PolarVortex attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <-
        select
          $ assetControlledBy iid
          <> AssetWithAnyRemainingHealth
          <> AssetCanBeDamagedBySource (attrs.ability 1)
      chooseOneAtATimeM iid do
        targeting iid $ directDamage iid (attrs.ability 1) 1
        for_ assets \asset -> do
          targeting asset $ push $ DealAssetDamage asset (attrs.ability 1) 1 0
      pure t
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> PolarVortex <$> liftRunMessage msg attrs
