module Arkham.Location.Cards.ChapultepecHill_179 (
  chapultepecHill_179,
  ChapultepecHill_179 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype ChapultepecHill_179 = ChapultepecHill_179 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecHill_179 :: LocationCard ChapultepecHill_179
chapultepecHill_179 =
  locationWith
    ChapultepecHill_179
    Cards.chapultepecHill_179
    4
    (PerPlayer 1)
    (labelL .~ "triangle")

instance HasModifiersFor ChapultepecHill_179 where
  getModifiersFor (InvestigatorTarget iid) (ChapultepecHill_179 a)
    | iid `on` a = pure $ toModifiers a [SkillModifier SkillWillpower (-2)]
  getModifiersFor _ _ = pure []

instance HasAbilities ChapultepecHill_179 where
  getAbilities (ChapultepecHill_179 attrs) =
    withRevealedAbilities
      attrs
      [ limitedAbility (GroupLimit PerPhase 1)
          $ restrictedAbility
            attrs
            1
            ( Here
                <> CluesOnThis (AtLeast $ Static 1)
                <> CanDiscoverCluesAt
                  (LocationWithId $ toId attrs)
            )
          $ ReactionAbility
            ( DrawCard
                Timing.After
                You
                (BasicCardMatch $ CardWithTrait Hex)
                AnyDeck
            )
            Free
      ]

instance RunMessage ChapultepecHill_179 where
  runMessage msg l@(ChapultepecHill_179 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorDiscoverClues iid (toId attrs) (toAbilitySource attrs 1) 1 Nothing
      pure l
    _ -> ChapultepecHill_179 <$> runMessage msg attrs
