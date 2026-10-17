module Arkham.Homebrew.AgesUnwound.Locations.FrenchHill (frenchHill) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)

newtype FrenchHill = FrenchHill LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill :: LocationCard FrenchHill
frenchHill = symbolLabel $ location FrenchHill Cards.frenchHill 6 (PerPlayer 1)

startedTurnHere :: ModifierType
startedTurnHere = ScenarioModifier "startedTurnAtFrenchHill"

instance HasAbilities FrenchHill where
  getAbilities (FrenchHill a) =
    extendRevealed
      a
      [ mkAbility a 1 $ silent $ TurnBegins #after (You <> at_ (be a))
      , restricted a 2 (DuringTurn You <> youExist (InvestigatorWithoutModifier startedTurnHere))
          $ silent
          $ InitiatedSkillTest #when You #any #any (WhileInvestigating $ be a)
      ]

instance RunMessage FrenchHill where
  runMessage msg l@(FrenchHill attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier iid (attrs.ability 1) iid startedTurnHere
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) attrs (ShroudModifier (-3))
      pure l
    _ -> FrenchHill <$> liftRunMessage msg attrs
