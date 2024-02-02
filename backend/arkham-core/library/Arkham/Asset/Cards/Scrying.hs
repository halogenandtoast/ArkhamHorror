module Arkham.Asset.Cards.Scrying (
  Scrying (..),
  scrying,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype Scrying = Scrying AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

scrying :: AssetCard Scrying
scrying = asset Scrying Cards.scrying

instance HasAbilities Scrying where
  getAbilities (Scrying a) =
    [ controlledAbility
        a
        1
        (exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ actionAbilityWithCost
        $ assetUseCost a Charge 1
        <> exhaust a
    ]

instance RunMessage Scrying where
  runMessage msg a@(Scrying attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      targets <- selectTargets =<< guardAffectsOthers iid can.manipulate.deck
      hasEncounterDeck <- can.target.encounterDeck iid
      let source = toAbilitySource attrs 1
      player <- getPlayer iid
      push
        $ chooseOne player
        $ targetLabels ([EncounterDeckTarget | hasEncounterDeck] <> targets)
        $ \target -> only $ lookAt iid source target [(FromTopOfDeck 3, PutBackInAnyOrder)] AnyCard ReturnCards
      pure a
    _ -> Scrying <$> runMessage msg attrs
