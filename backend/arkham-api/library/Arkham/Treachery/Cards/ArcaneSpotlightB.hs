module Arkham.Treachery.Cards.ArcaneSpotlightB (arcaneSpotlightB) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArcaneSpotlightB = ArcaneSpotlightB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneSpotlightB :: TreacheryCard ArcaneSpotlightB
arcaneSpotlightB = treachery ArcaneSpotlightB Cards.arcaneSpotlightB

instance HasAbilities ArcaneSpotlightB where
  getAbilities (ArcaneSpotlightB a) = case a.placement of
    AttachedToLocation lid ->
      [ mkAbility a 1 $ forced $ Enters #after You (LocationWithId lid)
      , mkAbility a 2 $ forced $ RoundEnds #when
      ]
    _ -> []

instance RunMessage ArcaneSpotlightB where
  runMessage msg t@(ArcaneSpotlightB attrs) = runQueueT $ case msg of
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
    _ -> ArcaneSpotlightB <$> liftRunMessage msg attrs
