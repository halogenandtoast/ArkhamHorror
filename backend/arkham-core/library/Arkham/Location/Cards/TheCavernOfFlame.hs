module Arkham.Location.Cards.TheCavernOfFlame (
  theCavernOfFlame,
  TheCavernOfFlame (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype TheCavernOfFlame = TheCavernOfFlame LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theCavernOfFlame :: LocationCard TheCavernOfFlame
theCavernOfFlame = location TheCavernOfFlame Cards.theCavernOfFlame 9 (Static 0)

instance HasModifiersFor TheCavernOfFlame where
  getModifiersFor target (TheCavernOfFlame a) | a `is` target = do
    blocked <- selectAny $ locationIs Cards.seventySteps <> LocationWithAnyClues
    pure $ toModifiers a [Blocked | blocked]
  getModifiersFor _ _ = pure []

instance HasAbilities TheCavernOfFlame where
  getAbilities (TheCavernOfFlame attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ investigatorAt (toId attrs))
          $ ForcedAbility
          $ PhaseEnds #when #mythos
      ]

instance RunMessage TheCavernOfFlame where
  runMessage msg l@(TheCavernOfFlame attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- selectList $ investigatorAt (toId attrs)
      lead <- getLeadPlayer
      push
        $ chooseOrRunOneAtATime
          lead
          [targetLabel iid [assignDamage iid (toAbilitySource attrs 1) 1] | iid <- investigators]
      pure l
    _ -> TheCavernOfFlame <$> runMessage msg attrs
