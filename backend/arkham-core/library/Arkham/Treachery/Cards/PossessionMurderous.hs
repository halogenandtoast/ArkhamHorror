module Arkham.Treachery.Cards.PossessionMurderous (
  possessionMurderous,
  PossessionMurderous (..),
) where

import Arkham.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (TreacheryInHandOf)
import Arkham.Placement
import Arkham.Projection
import Arkham.Strategy
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PossessionMurderous = PossessionMurderous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionMurderous :: TreacheryCard PossessionMurderous
possessionMurderous = treachery PossessionMurderous Cards.possessionMurderous

instance HasAbilities PossessionMurderous where
  getAbilities (PossessionMurderous a) =
    [ restrictedAbility a 1 InYourHand
        $ actionAbilityWithCost
          (InvestigatorDamageCost (toSource a) (InvestigatorAt YourLocation) DamageAny 2)
    ]

instance RunMessage PossessionMurderous where
  runMessage msg t@(PossessionMurderous attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      when (horror > sanity * 2) $ kill attrs iid
      placeTreachery attrs (HiddenInHand iid)
      pure t
    EndCheckWindow {} -> case attrs.placement of
      HiddenInHand iid -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        when (horror > sanity * 2) $ kill attrs iid
        pure t
      _ -> pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PossessionMurderous <$> liftRunMessage msg attrs
