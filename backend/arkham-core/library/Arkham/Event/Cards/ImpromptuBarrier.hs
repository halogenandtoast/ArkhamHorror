module Arkham.Event.Cards.ImpromptuBarrier (
  impromptuBarrier,
  ImpromptuBarrier (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types qualified as Enemy (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.SkillType
import Arkham.Zone

newtype Metadata = Metadata {fromDiscard :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ImpromptuBarrier = ImpromptuBarrier (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impromptuBarrier :: EventCard ImpromptuBarrier
impromptuBarrier =
  event (ImpromptuBarrier . (`with` Metadata False)) Cards.impromptuBarrier

instance RunMessage ImpromptuBarrier where
  runMessage msg e@(ImpromptuBarrier (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ _ zone | eid == toId attrs -> do
      pushAll
        $ [ ChooseEvadeEnemy
              iid
              (toSource attrs)
              (Just $ toTarget attrs)
              SkillCombat
              mempty
              False
          ]
        <> [ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs) | zone == FromDiscard]
      pure . ImpromptuBarrier $ attrs `with` Metadata (zone == FromDiscard)
    ChosenEvadeEnemy (isSource attrs -> True) eid -> do
      push $ skillTestModifier attrs (EnemyTarget eid) (EnemyEvade (-1))
      pure e
    Successful (Action.Evade, EnemyTarget enemyId) iid _ (isTarget attrs -> True) n ->
      do
        enemies <-
          selectWithField Enemy.EnemyEvade
            $ EnemyAt (locationWithEnemy enemyId)
            <> NotEnemy (EnemyWithId enemyId)
        let enemies' = map fst $ filter (\(_, x) -> maybe False (<= n) x) enemies
        pushAll
          $ EnemyEvaded iid enemyId
          : [ chooseOne
              iid
              [ Label "Do not evade another enemy" []
              , Label
                  "Evade Another Enemy"
                  [ ChooseEvadeEnemy
                      iid
                      (toSource attrs)
                      Nothing
                      SkillCombat
                      (EnemyOneOf $ map EnemyWithId enemies')
                      False
                  ]
              ]
            | notNull enemies'
            ]

        pure e
    _ -> ImpromptuBarrier . (`with` meta) <$> runMessage msg attrs
