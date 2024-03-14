module Arkham.Location.Cards.StudentUnion (StudentUnion (..), studentUnion) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards (studentUnion)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype StudentUnion = StudentUnion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studentUnion :: LocationCard StudentUnion
studentUnion = location StudentUnion Cards.studentUnion 1 (Static 2)

instance HasAbilities StudentUnion where
  getAbilities (StudentUnion attrs) =
    extendRevealed
      attrs
      [ mkAbility attrs 1 $ forced $ RevealLocation #after Anyone $ be attrs
      , restrictedAbility
          attrs
          2
          ( Here
              <> exists
                ( oneOf
                    [HealableInvestigator (toSource attrs) hType You | hType <- [#horror, #damage]]
                )
          )
          $ ActionAbility []
          $ ActionCost 2
      ]

instance RunMessage StudentUnion where
  runMessage msg l@(StudentUnion attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
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
