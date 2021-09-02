module Arkham.Types.Location.Cards.Gondola
  ( gondola
  , Gondola(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Gondola = Gondola LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gondola :: LocationCard Gondola
gondola = location Gondola Cards.gondola 5 (Static 0) NoSymbol []

instance HasAbilities Gondola where
  getAbilities (Gondola x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here $ ActionAbility Nothing $ ActionCost 1
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env Gondola where
  runMessage msg l@(Gondola attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      locationIds <-
        setToList . deleteSet (toId attrs) <$> getSet @LocationId ()
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
