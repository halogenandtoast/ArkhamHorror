module Arkham.Asset.Cards.Scrying3 (
  Scrying3 (..),
  scrying3,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Trait

newtype Scrying3 = Scrying3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrying3 :: AssetCard Scrying3
scrying3 = asset Scrying3 Cards.scrying3

instance HasAbilities Scrying3 where
  getAbilities (Scrying3 a) =
    [ controlledAbility
        a
        1
        (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ FastAbility
        $ Costs
          [UseCost (AssetWithId $ toId a) Charge 1, ExhaustCost $ toTarget a]
    ]

instance RunMessage Scrying3 where
  runMessage msg a@(Scrying3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      targets <- selectTargets =<< guardAffectsOthers iid can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ TargetLabel
            target
            [lookAt iid source target [(FromTopOfDeck 3, PutBackInAnyOrder)] #any (defer attrs IsNotDraw)]
          | target <- [EncounterDeckTarget | hasEncounterDeck] <> targets
          ]
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      when (any (\c -> any (`elem` toTraits c) [Omen, Terror]) cards)
        $ push
        $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
      pure a
    _ -> Scrying3 <$> runMessage msg attrs
