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
      [ restrictedAbility attrs 1 (not_ $ exists $ LocationWithTitle "Dormitories") $ forced $ RevealLocation #after Anyone $ be attrs
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
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      whenM (selectNone $ LocationWithTitle "Dormitories") do
        push $ PlaceLocationMatching $ CardWithTitle "Dormitories"
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      let source = attrs.ability 2
      healDamage <- canHaveDamageHealed source iid
      healHorror <- canHaveHorrorHealed source iid
      pushAll
        $ [ HealDamage (InvestigatorTarget iid) source 1
          | healDamage
          ]
        <> [ HealHorror (InvestigatorTarget iid) source 1
           | healHorror
           ]
      pure l
    _ -> StudentUnion <$> runMessage msg attrs
