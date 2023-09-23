module Arkham.Location.Cards.UnvisitedIsleForsakenWoods (
  unvisitedIsleForsakenWoods,
  UnvisitedIsleForsakenWoods (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype UnvisitedIsleForsakenWoods = UnvisitedIsleForsakenWoods LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleForsakenWoods :: LocationCard UnvisitedIsleForsakenWoods
unvisitedIsleForsakenWoods = location UnvisitedIsleForsakenWoods Cards.unvisitedIsleForsakenWoods 2 (PerPlayer 2)

instance HasModifiersFor UnvisitedIsleForsakenWoods where
  getModifiersFor target (UnvisitedIsleForsakenWoods attrs)
    | attrs `isTarget` target
    , not (locationRevealed attrs) = do
        sidedWithLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
        isLit <- selectAny $ locationIs Locations.forbiddingShore <> LocationWithBrazier Lit
        pure
          [ toModifier attrs Blocked
          | if sidedWithLodge then not isLit else isLit
          ]
  getModifiersFor _ _ = pure []

instance HasAbilities UnvisitedIsleForsakenWoods where
  getAbilities (UnvisitedIsleForsakenWoods attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here $ ActionAbility (Just Action.Circle) $ ActionCost 1
      , haunted
          "You must either search the encounter deck and discard pile for a Whippoorwill and spawn it at this location, or the nearest Whippoorwill attack you."
          attrs
          2
      ]

instance RunMessage UnvisitedIsleForsakenWoods where
  runMessage msg l@(UnvisitedIsleForsakenWoods attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      circleTest iid attrs attrs [#willpower, #combat] 11
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      whippoorwills <- selectList $ NearestEnemy $ enemyIs Enemies.whippoorwill

      push
        $ chooseOrRunOne iid
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
                iid
                [ targetLabel whippoorwill [EnemyAttack $ enemyAttack whippoorwill attrs iid]
                | whippoorwill <- whippoorwills
                ]
            ]
          | notNull whippoorwills
          ]
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      passedCircleTest iid attrs
      pure l
    _ -> UnvisitedIsleForsakenWoods <$> runMessage msg attrs
