module Arkham.Homebrew.AgesUnwound.Locations.IndependenceSquare (independenceSquare) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype IndependenceSquare = IndependenceSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

independenceSquare :: LocationCard IndependenceSquare
independenceSquare = symbolLabel $ location IndependenceSquare Cards.independenceSquare 2 (PerPlayer 2)

{- | "Forced - When an investigator at this location initiates a skill test
printed on an act/agenda card: Increase the difficulty of this skill test by 2."

Every act/agenda test in this scenario comes from an ability on the card, and an
ability source reports the card's own type, so @SourceIsType@ catches it.
-}
instance HasAbilities IndependenceSquare where
  getAbilities (IndependenceSquare a) =
    extendRevealed1 a
      $ forcedAbility a 1
      $ InitiatedSkillTest #when (You <> at_ (be a)) #any #any
      $ SkillTestSourceMatches
      $ oneOf [SourceIsType ActType, SourceIsType AgendaType]

instance RunMessage IndependenceSquare where
  runMessage msg l@(IndependenceSquare attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty 2)
      pure l
    _ -> IndependenceSquare <$> liftRunMessage msg attrs
