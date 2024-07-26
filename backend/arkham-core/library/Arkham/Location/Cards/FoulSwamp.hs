module Arkham.Location.Cards.FoulSwamp (
  FoulSwamp (..),
  foulSwamp,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (foulSwamp)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.ScenarioLogKey

newtype FoulSwamp = FoulSwamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulSwamp :: LocationCard FoulSwamp
foulSwamp = location FoulSwamp Cards.foulSwamp 2 (Static 0)

instance HasModifiersFor FoulSwamp where
  getModifiersFor (InvestigatorTarget iid) (FoulSwamp attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs $ guard here *> [CannotHealHorror, CannotCancelHorror]
  getModifiersFor _ _ = pure []

instance HasAbilities FoulSwamp where
  getAbilities (FoulSwamp attrs) =
    withRevealedAbilities attrs
      $ [ skillTestAbility
            $ restrictedAbility attrs 1 Here
            $ ActionAbility []
            $ Costs [ActionCost 1, UpTo (Fixed 3) (HorrorCost (toSource attrs) YouTarget 1)]
        ]

instance RunMessage FoulSwamp where
  runMessage msg l@(FoulSwamp attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let
        horrorPayment = \case
          Payments ps -> foldMap horrorPayment ps
          HorrorPayment a -> Sum a
          _ -> Sum 0
        n = getSum $ horrorPayment payments
        source = toAbilitySource attrs 1
      sid <- getRandom
      pushAll
        [ skillTestModifier sid source iid $ SkillModifier #willpower n
        , beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 7)
        ]
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ Remember FoundAnAncientBindingStone
      pure l
    _ -> FoulSwamp <$> runMessage msg attrs
