module Arkham.Asset.Assets.AlyssaGraham (alyssaGraham) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype AlyssaGraham = AlyssaGraham AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alyssaGraham :: AssetCard AlyssaGraham
alyssaGraham = ally AlyssaGraham Cards.alyssaGraham (1, 3)

instance HasAbilities AlyssaGraham where
  getAbilities (AlyssaGraham a) =
    [ controlled a 1 (any_ [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck])
        $ FastAbility (exhaust a)
    ]

instance HasModifiersFor AlyssaGraham where
  getModifiersFor (AlyssaGraham a) = controllerGets a [SkillModifier #intellect 1]

instance RunMessage AlyssaGraham where
  runMessage msg a@(AlyssaGraham attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      chooseTargetM iid (#encounterDeck : map toTarget investigators) \t -> do
        lookAt iid (attrs.ability 1) t [(FromTopOfDeck 1, PutBack)] #any (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) deck cards -> do
      canAffectOtherPlayers <- can.affect.otherPlayers iid
      focusCards cards do
        chooseOrRunOneM iid do
          when canAffectOtherPlayers do
            labeled "Add 1 Doom to Alyssa to move card to bottom" do
              placeDoom (attrs.ability 1) attrs 1
              for_ cards $ putCardOnBottomOfDeck iid deck
          labeled "Leave card on top" nothing
      pure a
    _ -> AlyssaGraham <$> liftRunMessage msg attrs
