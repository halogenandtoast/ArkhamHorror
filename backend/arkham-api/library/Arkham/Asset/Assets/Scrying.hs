module Arkham.Asset.Assets.Scrying (scrying) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype Scrying = Scrying AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrying :: AssetCard Scrying
scrying = asset Scrying Cards.scrying

instance HasAbilities Scrying where
  getAbilities (Scrying a) =
    [ controlled a 1 (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ actionAbilityWithCost (assetUseCost a #charge 1 <> exhaust a)
    ]

instance RunMessage Scrying where
  runMessage msg a@(Scrying attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      decks <- selectTargets =<< guardAffectsOthers iid can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      chooseOneM iid do
        targets ([EncounterDeckTarget | hasEncounterDeck] <> decks) \target ->
          lookAt iid (attrs.ability 1) target [(FromTopOfDeck 3, PutBackInAnyOrder)] #any ReturnCards
      pure a
    _ -> Scrying <$> liftRunMessage msg attrs
