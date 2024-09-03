module Arkham.Asset.Cards.PatricesViolin (
  patricesViolin,
  PatricesViolin (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Matcher

newtype PatricesViolin = PatricesViolin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patricesViolin :: AssetCard PatricesViolin
patricesViolin = asset PatricesViolin Cards.patricesViolin

instance HasAbilities PatricesViolin where
  getAbilities (PatricesViolin x) =
    [ controlledAbility x 1 (atYourLocation $ affectsOthers $ oneOf [can.gain.resources, can.draw.cards])
        $ FastAbility (exhaust x <> HandDiscardCost 1 #any)
    ]

instance RunMessage PatricesViolin where
  runMessage msg a@(PatricesViolin attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <-
        select $ affectsOthers $ colocatedWith iid <> oneOf [can.gain.resources, can.draw.cards]
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ targetLabels investigators (only . HandleTargetChoice iid (toAbilitySource attrs 1) . toTarget)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      let source = toAbilitySource attrs 1
      canGainResources <- can.gain.resources iid'
      canDrawCards <- can.draw.cards iid'
      let drawing = drawCards iid' source 1
      player <- getPlayer iid

      push
        $ chooseOne player
        $ [Label "Gain resource" [takeResources iid' source 1] | canGainResources]
        <> [Label "Draw card" [drawing] | canDrawCards]

      pure a
    _ -> PatricesViolin <$> runMessage msg attrs
