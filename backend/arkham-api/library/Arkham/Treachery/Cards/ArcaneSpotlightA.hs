module Arkham.Treachery.Cards.ArcaneSpotlightA (arcaneSpotlightA) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArcaneSpotlightA = ArcaneSpotlightA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneSpotlightA :: TreacheryCard ArcaneSpotlightA
arcaneSpotlightA = treachery ArcaneSpotlightA Cards.arcaneSpotlightA

instance HasAbilities ArcaneSpotlightA where
  getAbilities (ArcaneSpotlightA a) = case a.placement of
    AttachedToLocation lid ->
      [ mkAbility a 1 $ forced $ Enters #after You (LocationWithId lid)
      , mkAbility a 2 $ forced $ RoundEnds #when
      ]
    _ -> []

instance RunMessage ArcaneSpotlightA where
  runMessage msg t@(ArcaneSpotlightA attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <-
        select
          $ NearestLocationTo iid
          $ LocationWithoutTreachery
          $ mapOneOf treacheryIs [Cards.arcaneSpotlightA, Cards.arcaneSpotlightB, Cards.arcaneSpotlightC]
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1) [iid]
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      case attrs.placement of
        AttachedToLocation lid -> do
          raiseAlarmLevel (attrs.ability 1) =<< select (InvestigatorAt $ LocationWithId lid)
        _ -> pure ()
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ArcaneSpotlightA <$> liftRunMessage msg attrs
