module Arkham.Asset.Assets.AbyssalTome2 (abyssalTome2, AbyssalTome2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude
import Arkham.Projection

newtype AbyssalTome2 = AbyssalTome2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalTome2 :: AssetCard AbyssalTome2
abyssalTome2 = asset AbyssalTome2 Cards.abyssalTome2

instance HasAbilities AbyssalTome2 where
  getAbilities (AbyssalTome2 attrs) =
    [restrictedAbility attrs 1 ControlsThis $ fightAction (exhaust attrs)]

instance HasModifiersFor AbyssalTome2 where
  getModifiersFor (AbyssalTome2 a) = case a.controller of
    Just iid -> maybeModified_ a iid do
      source <- MaybeT getSkillTestSource
      guard $ isAbilitySource a 1 source
      doom <- lift $ field AssetDoom a.id
      pure [AnySkillValue doom, DamageDealt doom]
    _ -> pure mempty

instance RunMessage AbyssalTome2 where
  runMessage msg a@(AbyssalTome2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid

      sid <- getRandom
      choices <- for [#intellect, #willpower, #combat] \sType -> do
        chooseFight <- withSkillType sType <$> mkChooseFight sid iid (attrs.ability 1)
        pure $ SkillLabel sType [toMessage chooseFight]

      chooseMsg <-
        chooseAmounts player "Amount of Doom to Place" (MaxAmountTarget 3) [("Doom", (0, 3))] attrs

      pushAll
        [ chooseMsg
        , chooseOne player choices
        ]
      pure a
    ResolveAmounts _ (getChoiceAmount "Doom" -> n) (isTarget attrs -> True) -> do
      push $ PlaceDoom (attrs.ability 1) (toTarget attrs) n
      pure a
    _ -> AbyssalTome2 <$> runMessage msg attrs
