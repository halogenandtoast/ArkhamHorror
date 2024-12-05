module Arkham.Location.Cards.UnvisitedIsleForsakenWoods (
  unvisitedIsleForsakenWoods,
  UnvisitedIsleForsakenWoods (..),
)
where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleForsakenWoods = UnvisitedIsleForsakenWoods LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleForsakenWoods :: LocationCard UnvisitedIsleForsakenWoods
unvisitedIsleForsakenWoods = location UnvisitedIsleForsakenWoods Cards.unvisitedIsleForsakenWoods 2 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleForsakenWoods where
  getModifiersFor (UnvisitedIsleForsakenWoods a) = whenUnrevealed a $ maybeModifySelf a do
    sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleForsakenWoods where
  getAbilities (UnvisitedIsleForsakenWoods attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ ActionAbility [Action.Circle] $ ActionCost 1
      , haunted
          "You must either search the encounter deck and discard pile for a Whippoorwill and spawn it at this location, or the nearest Whippoorwill attack you."
          attrs
          2
      ]

instance RunMessage UnvisitedIsleForsakenWoods where
  runMessage msg l@(UnvisitedIsleForsakenWoods attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      circleTest sid iid (attrs.ability 1) attrs [#willpower, #combat] (Fixed 11)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      whippoorwills <- select $ NearestEnemyTo iid $ enemyIs Enemies.whippoorwill
      chooseOrRunOneM iid do
        labeled
          "Search the encounter deck and discard pile for a Whippoorwill and spawn it at this location"
          do
            findEncounterCard iid attrs $ cardIs Enemies.whippoorwill
        unless (null whippoorwills) do
          labeled "The nearest Whippoorwill attacks you" do
            chooseOrRunOneM iid do
              targets whippoorwills \whippoorwill -> initiateEnemyAttack whippoorwill attrs iid
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyAt_ card attrs
      pure l
    _ -> UnvisitedIsleForsakenWoods <$> liftRunMessage msg attrs
