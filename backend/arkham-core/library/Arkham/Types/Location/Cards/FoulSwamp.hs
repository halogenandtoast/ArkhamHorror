module Arkham.Types.Location.Cards.FoulSwamp
  ( FoulSwamp(..)
  , foulSwamp
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (foulSwamp)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Target
import Data.Semigroup

newtype FoulSwamp = FoulSwamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulSwamp :: LocationCard FoulSwamp
foulSwamp = location
  FoulSwamp
  Cards.foulSwamp
  2
  (Static 0)
  Hourglass
  [Equals, Square, Triangle, Diamond]

instance HasModifiersFor env FoulSwamp where
  getModifiersFor _ (InvestigatorTarget iid) (FoulSwamp attrs)
    | iid `member` locationInvestigators attrs = pure
    $ toModifiers attrs [CannotHealHorror, CannotCancelHorror]
  getModifiersFor _ _ _ = pure []

instance HasAbilities FoulSwamp where
  getAbilities (FoulSwamp attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            Here
            (ActionAbility Nothing $ Costs
              [ActionCost 1, UpTo 3 (HorrorCost (toSource attrs) YouTarget 1)]
            )
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage env FoulSwamp where
  runMessage msg l@(FoulSwamp attrs) = case msg of
    UseCardAbility iid source _ 1 payments | isSource attrs source -> do
      let
        horrorPayment = \case
          Payments ps -> foldMap horrorPayment ps
          HorrorPayment a -> Sum a
          _ -> Sum 0
        n = getSum $ horrorPayment payments
      l <$ pushAll
        [ skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower n)
        , BeginSkillTest iid source (toTarget attrs) Nothing SkillWillpower 7
        ]
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember FoundAnAncientBindingStone)
    _ -> FoulSwamp <$> runMessage msg attrs
