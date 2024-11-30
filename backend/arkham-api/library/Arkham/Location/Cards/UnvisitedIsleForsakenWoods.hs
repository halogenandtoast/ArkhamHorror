module Arkham.Location.Cards.UnvisitedIsleForsakenWoods (
  unvisitedIsleForsakenWoods,
  UnvisitedIsleForsakenWoods (..),
)
where

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleForsakenWoods = UnvisitedIsleForsakenWoods LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleForsakenWoods :: LocationCard UnvisitedIsleForsakenWoods
unvisitedIsleForsakenWoods = location UnvisitedIsleForsakenWoods Cards.unvisitedIsleForsakenWoods 2 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleForsakenWoods where
  getModifiersFor target (UnvisitedIsleForsakenWoods attrs) = maybeModified attrs do
    guard $ attrs `isTarget` target
    guard $ not attrs.revealed
    sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
    isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
    guard $ if sidedWithLodge then not isLit else isLit
    pure [Blocked]

instance HasAbilities UnvisitedIsleForsakenWoods where
  getAbilities (UnvisitedIsleForsakenWoods attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 Here $ ActionAbility ([Action.Circle]) $ ActionCost 1
      , haunted
          "You must either search the encounter deck and discard pile for a Whippoorwill and spawn it at this location, or the nearest Whippoorwill attack you."
          attrs
          2
      ]

instance RunMessage UnvisitedIsleForsakenWoods where
  runMessage msg l@(UnvisitedIsleForsakenWoods attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- genId
      circleTest sid iid (attrs.ability 1) attrs [#willpower, #combat] (Fixed 11)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      whippoorwills <- select $ NearestEnemyTo iid $ enemyIs Enemies.whippoorwill
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Search the encounter deck and discard pile for a Whippoorwill and spawn it at this location"
          [ FindEncounterCard
              iid
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard]
              $ cardIs Enemies.whippoorwill
          ]
        : [ Label
            "The nearest Whippoorwill attacks you"
            [ chooseOrRunOne
                player
                [ targetLabel whippoorwill [EnemyAttack $ enemyAttack whippoorwill attrs iid]
                | whippoorwill <- whippoorwills
                ]
            ]
          | notNull whippoorwills
          ]
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      passedCircleTest iid attrs
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      pushM $ createEnemyAt_ card (toId attrs) Nothing
      pure l
    _ -> UnvisitedIsleForsakenWoods <$> runMessage msg attrs
