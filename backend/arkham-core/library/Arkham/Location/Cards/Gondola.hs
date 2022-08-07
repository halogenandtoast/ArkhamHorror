module Arkham.Location.Cards.Gondola
  ( gondola
  , Gondola(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype Gondola = Gondola LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gondola :: LocationCard Gondola
gondola = location Gondola Cards.gondola 5 (Static 0)

instance HasAbilities Gondola where
  getAbilities (Gondola x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here $ ActionAbility Nothing $ ActionCost 1
    | locationRevealed x
    ]

instance RunMessage Gondola where
  runMessage msg l@(Gondola attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      locationIds <- setToList . deleteSet (toId attrs) <$> select Anywhere
      l <$ pushAll
        (MoveAllTo (toSource attrs) (toId attrs)
        : [ RemoveLocation lid | lid <- locationIds ]
        )
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ push
        (chooseOne
          iid
          [ Label
            "Test {combat} (2)"
            [ BeginSkillTest
                iid
                (toSource attrs)
                (toTarget attrs)
                Nothing
                SkillCombat
                2
            ]
          , Label
            "Test {agility} (2)"
            [ BeginSkillTest
                iid
                (toSource attrs)
                (toTarget attrs)
                Nothing
                SkillAgility
                2
            ]
          ]
        )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (PlaceResources (toTarget attrs) 1)
    _ -> Gondola <$> runMessage msg attrs
