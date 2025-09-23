module Arkham.Treachery.Cards.UnstableEnergies (unstableEnergies) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnstableEnergies = UnstableEnergies TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unstableEnergies :: TreacheryCard UnstableEnergies
unstableEnergies = treachery UnstableEnergies Cards.unstableEnergies

instance HasAbilities UnstableEnergies where
  getAbilities (UnstableEnergies a) =
    [ mkAbility a 1 $ forced $ Leaves #when You (LocationWithTreachery (be a))
    , restricted a 2 (thisExists a $ TreacheryAttachedToLocation LocationWithoutInvestigators)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage UnstableEnergies where
  runMessage msg t@(UnstableEnergies attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.location $ runLocationHauntedAbilities iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> UnstableEnergies <$> liftRunMessage msg attrs
