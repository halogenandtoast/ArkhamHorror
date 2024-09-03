module Arkham.Location.Cards.ChapultepecHill_179 (
  chapultepecHill_179,
  ChapultepecHill_179 (..),
) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Trait

newtype ChapultepecHill_179 = ChapultepecHill_179 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecHill_179 :: LocationCard ChapultepecHill_179
chapultepecHill_179 =
  locationWith ChapultepecHill_179 Cards.chapultepecHill_179 4 (PerPlayer 1) (labelL .~ "triangle")

instance HasModifiersFor ChapultepecHill_179 where
  getModifiersFor (InvestigatorTarget iid) (ChapultepecHill_179 a) = do
    here <- iid `isAt` a
    pure $ toModifiers a [SkillModifier #willpower (-2) | here]
  getModifiersFor _ _ = pure []

instance HasAbilities ChapultepecHill_179 where
  getAbilities (ChapultepecHill_179 attrs) =
    extendRevealed
      attrs
      [ groupLimit PerPhase
          $ restrictedAbility
            attrs
            1
            (Here <> CluesOnThis (atLeast 1) <> CanDiscoverCluesAt (LocationWithId attrs.id))
          $ freeReaction
          $ DrawCard #after You (basic $ CardWithTrait Hex) AnyDeck
      ]

instance RunMessage ChapultepecHill_179 where
  runMessage msg l@(ChapultepecHill_179 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) 1
      pure l
    _ -> ChapultepecHill_179 <$> runMessage msg attrs
