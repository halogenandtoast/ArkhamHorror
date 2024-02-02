module Arkham.Location.Cards.StudentUnion (
  StudentUnion (..),
  studentUnion,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards (studentUnion)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype StudentUnion = StudentUnion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

studentUnion :: LocationCard StudentUnion
studentUnion = location StudentUnion Cards.studentUnion 1 (Static 2)

instance HasAbilities StudentUnion where
  getAbilities (StudentUnion attrs) =
    withBaseAbilities attrs
      $ if locationRevealed attrs
        then
          [ mkAbility attrs 1
              $ ForcedAbility
              $ RevealLocation Timing.After Anyone
              $ LocationWithId
              $ toId attrs
          , restrictedAbility
              attrs
              2
              ( Here
                  <> InvestigatorExists
                    ( AnyInvestigator
                        [ HealableInvestigator (toSource attrs) HorrorType You
                        , HealableInvestigator (toSource attrs) DamageType You
                        ]
                    )
              )
              $ ActionAbility []
              $ ActionCost 2
          ]
        else []

instance RunMessage StudentUnion where
  runMessage msg l@(StudentUnion attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          l <$ push (PlaceLocationMatching $ CardWithTitle "Dormitories")
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      healDamage <- canHaveDamageHealed source iid
      mHealHorror <- getHealHorrorMessage source 1 iid
      pushAll
        $ [ HealDamage (InvestigatorTarget iid) (toSource attrs) 1
          | healDamage
          ]
        <> maybeToList mHealHorror
      pure l
    _ -> StudentUnion <$> runMessage msg attrs
