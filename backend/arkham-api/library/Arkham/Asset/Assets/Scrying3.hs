module Arkham.Asset.Assets.Scrying3 (scrying3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Trait

newtype Scrying3 = Scrying3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrying3 :: AssetCard Scrying3
scrying3 = asset Scrying3 Cards.scrying3

instance HasAbilities Scrying3 where
  getAbilities (Scrying3 a) =
    [ controlled a 1 (exists $ oneOf [affectsOthers can.manipulate.deck, can.target.encounterDeck You])
        $ FastAbility (assetUseCost a Charge 1 <> exhaust a)
    ]

instance RunMessage Scrying3 where
  runMessage msg a@(Scrying3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- selectTargets =<< guardAffectsOthers iid can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      chooseOneM iid do
        targets ([EncounterDeckTarget | hasEncounterDeck] <> investigators) \target -> do
          lookAt
            iid
            (attrs.ability 1)
            target
            [(FromTopOfDeck 3, PutBackInAnyOrder)]
            #any
            (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      when (any (\c -> any (`elem` toTraits c) [Omen, Terror]) cards) $ assignHorror iid attrs 1
      pure a
    _ -> Scrying3 <$> liftRunMessage msg attrs
