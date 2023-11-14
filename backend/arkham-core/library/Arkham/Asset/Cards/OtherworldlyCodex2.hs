module Arkham.Asset.Cards.OtherworldlyCodex2 (
  otherworldlyCodex2,
  OtherworldlyCodex2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Card
import Arkham.Matcher
import Arkham.Trait (Trait (Elite))

newtype OtherworldlyCodex2 = OtherworldlyCodex2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyCodex2 :: AssetCard OtherworldlyCodex2
otherworldlyCodex2 = asset OtherworldlyCodex2 Cards.otherworldlyCodex2

instance HasAbilities OtherworldlyCodex2 where
  getAbilities (OtherworldlyCodex2 attrs) =
    [ controlledAbility attrs 1 (exists $ You <> can.target.encounterDeck)
        $ actionAbilityWithCost
        $ exhaust attrs
        <> assetUseCost attrs Secret 1
    ]

instance RunMessage OtherworldlyCodex2 where
  runMessage msg a@(OtherworldlyCodex2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ search
          iid
          (toAbilitySource attrs 1)
          EncounterDeckTarget
          [fromTopOfDeck 9]
          (NotCard $ CardWithTrait Elite)
          (DeferSearchedToTarget $ toTarget attrs)
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      let defs = map toCardDef cards
      treacheries <- selectTargets $ oneOf $ map treacheryIs defs
      enemies <- selectTargets $ oneOf $ map enemyIs defs
      locations <- selectTargets $ oneOf $ map locationIs defs
      assets <- selectTargets $ oneOf $ map assetIs defs
      -- [ALERT] EncounterDeckTypes
      let targets = treacheries <> enemies <> locations <> assets

      player <- getPlayer iid

      pushIfAny targets
        $ chooseOne player
        $ targetLabels targets
        $ only
        . toDiscardBy iid (toAbilitySource attrs 1)

      pure a
    _ -> OtherworldlyCodex2 <$> runMessage msg attrs
