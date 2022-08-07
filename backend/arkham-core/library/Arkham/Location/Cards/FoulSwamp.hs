module Arkham.Location.Cards.FoulSwamp
  ( FoulSwamp(..)
  , foulSwamp
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( foulSwamp )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Target

newtype FoulSwamp = FoulSwamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foulSwamp :: LocationCard FoulSwamp
foulSwamp = location FoulSwamp Cards.foulSwamp 2 (Static 0)

instance HasModifiersFor FoulSwamp where
  getModifiersFor (InvestigatorTarget iid) (FoulSwamp attrs)
    | iid `member` locationInvestigators attrs = pure
    $ toModifiers attrs [CannotHealHorror, CannotCancelHorror]
  getModifiersFor _ _ = pure []

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

instance RunMessage FoulSwamp where
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
