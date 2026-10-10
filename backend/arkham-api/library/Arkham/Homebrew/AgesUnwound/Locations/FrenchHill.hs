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

{- | Shroud 6, Victory 1 (the victory is on the card def): the one street worth
stopping in, and only if you got here this turn.
-}
frenchHill :: LocationCard FrenchHill
frenchHill = symbolLabel $ location FrenchHill Cards.frenchHill 6 (PerPlayer 1)

{- | "While investigating French Hill during your turn, if you did not start your
turn here, it gets -3 shroud."

Two engine-only hooks, neither of them printed as an ability:

* 1 marks the investigators who /did/ start their turn here, because nothing in
  the engine records where a turn began. A turn-scoped 'ScenarioModifier' is the
  established way to leave such a mark (Cthulhu's patrol marker), and it is read
  back through a matcher rather than @getModifiers@ so this instance does not
  have to query another entity's modifiers.
* 2 applies the shroud reduction to the investigation itself, the way a "while
  investigating this location" shroud change always is -- a plain
  'ShroudModifier' is global and could not be scoped to one investigator.
-}
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
