module Arkham.Asset.Cards.AlyssaGraham (alyssaGraham, AlyssaGraham (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Strategy

newtype AlyssaGraham = AlyssaGraham AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alyssaGraham :: AssetCard AlyssaGraham
alyssaGraham = ally AlyssaGraham Cards.alyssaGraham (1, 3)

instance HasAbilities AlyssaGraham where
  getAbilities (AlyssaGraham a) =
    [ controlledAbility a 1 (any_ [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ FastAbility (exhaust a)
    ]

instance HasModifiersFor AlyssaGraham where
  getModifiersFor (InvestigatorTarget iid) (AlyssaGraham a) =
    modified a [SkillModifier #intellect 1 | iid `controls` a]
  getModifiersFor _ _ = pure []

instance RunMessage AlyssaGraham where
  runMessage msg a@(AlyssaGraham attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      targets <- map toTarget <$> getInvestigatorIds
      let
        goSearch t =
          TargetLabel
            t
            [Msg.lookAt iid (attrs.ability 1) t [(FromTopOfDeck 1, PutBack)] #any (defer attrs IsNotDraw)]
      chooseOne iid $ goSearch #encounterDeck : map goSearch targets
      pure a
    SearchFound iid (isTarget attrs -> True) deck cards -> do
      canAffectOtherPlayers <- can.affect.otherPlayers iid
      push $ FocusCards cards
      chooseOrRunOne
        iid
        $ [ Label "Add 1 Doom to Alyssa to move card to bottom"
            $ [UnfocusCards, Msg.placeDoom (attrs.ability 1) attrs 1]
            <> map (PutCardOnBottomOfDeck iid deck) cards
          | canAffectOtherPlayers
          ]
        <> [Label "Leave card on top" [UnfocusCards]]
      pure a
    _ -> AlyssaGraham <$> liftRunMessage msg attrs
