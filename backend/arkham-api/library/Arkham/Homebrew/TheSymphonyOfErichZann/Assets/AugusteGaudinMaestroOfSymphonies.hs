module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.AugusteGaudinMaestroOfSymphonies (augusteGaudinMaestroOfSymphonies) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Strategy

newtype AugusteGaudinMaestroOfSymphonies = AugusteGaudinMaestroOfSymphonies AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augusteGaudinMaestroOfSymphonies :: AssetCard AugusteGaudinMaestroOfSymphonies
augusteGaudinMaestroOfSymphonies =
  ally AugusteGaudinMaestroOfSymphonies Cards.augusteGaudinMaestroOfSymphonies (2, 2)

instance HasAbilities AugusteGaudinMaestroOfSymphonies where
  getAbilities (AugusteGaudinMaestroOfSymphonies a) =
    [ restricted a 1 (youExist $ at_ (locationWithAsset a.id)) $ FastAbility (exhaust a)
    ]

instance RunMessage AugusteGaudinMaestroOfSymphonies where
  runMessage msg a@(AugusteGaudinMaestroOfSymphonies attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) EncounterDeckTarget [fromTopOfDeck 9] #any (DrawFound iid 1)
      search iid (attrs.ability 1) iid [fromTopOfDeck 3] #any (DrawFound iid 1)
      pure a
    _ -> AugusteGaudinMaestroOfSymphonies <$> liftRunMessage msg attrs
