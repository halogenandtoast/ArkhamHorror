module Arkham.Location.Cards.ArkhamWoodsQuietGlade (
  ArkhamWoodsQuietGlade (..),
  arkhamWoodsQuietGlade,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards (arkhamWoodsQuietGlade)
import Arkham.Location.Runner
import Arkham.Matcher

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsQuietGlade :: LocationCard ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade =
  location ArkhamWoodsQuietGlade Cards.arkhamWoodsQuietGlade 1 (Static 0)

instance HasAbilities ArkhamWoodsQuietGlade where
  getAbilities (ArkhamWoodsQuietGlade attrs) =
    withRevealedAbilities attrs
      $ [ limitedAbility (PlayerLimit PerTurn 1)
            $ withCriteria (mkAbility attrs 1 (ActionAbility [] $ ActionCost 1))
            $ Here
            <> InvestigatorExists
              ( AnyInvestigator
                  [ HealableInvestigator (toSource attrs) HorrorType You
                  , HealableInvestigator (toSource attrs) DamageType You
                  ]
              )
        ]

instance RunMessage ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = attrs.ability 1
      canHealDamage <- canHaveDamageHealed source iid
      canHealHorror <- canHaveHorrorHealed source iid
      pushAll
        $ [ HealDamage (InvestigatorTarget iid) source 1
          | canHealDamage
          ]
        <> [ HealHorror (InvestigatorTarget iid) source 1
           | canHealHorror
           ]
      pure l
    _ -> ArkhamWoodsQuietGlade <$> runMessage msg attrs
