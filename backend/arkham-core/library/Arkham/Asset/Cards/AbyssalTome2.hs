module Arkham.Asset.Cards.AbyssalTome2 (
  abyssalTome2,
  AbyssalTome2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Projection

newtype AbyssalTome2 = AbyssalTome2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalTome2 :: AssetCard AbyssalTome2
abyssalTome2 = asset AbyssalTome2 Cards.abyssalTome2

instance HasAbilities AbyssalTome2 where
  getAbilities (AbyssalTome2 attrs) =
    [ restrictedAbility attrs 1 ControlsThis $ fightAction (exhaust attrs)
    ]

instance HasModifiersFor AbyssalTome2 where
  getModifiersFor (InvestigatorTarget iid) (AbyssalTome2 attrs) | attrs `controlledBy` iid = do
    mSource <- getSkillTestSource
    case mSource of
      Just (isAbilitySource attrs 1 -> True) -> do
        doom <- field AssetDoom (toId attrs)
        pure $ toModifiers attrs [AnySkillValue doom, DamageDealt doom]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage AbyssalTome2 where
  runMessage msg a@(AbyssalTome2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      pushAll
        [ chooseAmounts player "Amount of Doom to Place" (MaxAmountTarget 3) [("Doom", (0, 3))] attrs
        , chooseOne
            player
            [ SkillLabel sType [chooseFightEnemy iid (toAbilitySource attrs 1) sType]
            | sType <- [#intellect, #willpower, #combat]
            ]
        ]
      pure a
    ResolveAmounts _ (getChoiceAmount "Doom" -> n) (isTarget attrs -> True) -> do
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) n
      pure a
    _ -> AbyssalTome2 <$> runMessage msg attrs
