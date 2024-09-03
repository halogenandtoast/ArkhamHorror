module Arkham.Event.Cards.ImpromptuBarrier (impromptuBarrier, ImpromptuBarrier (..)) where

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types qualified as Enemy (Field (..))
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Prelude
import Arkham.Zone

newtype Metadata = Metadata {fromDiscard :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ImpromptuBarrier = ImpromptuBarrier (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impromptuBarrier :: EventCard ImpromptuBarrier
impromptuBarrier = event (ImpromptuBarrier . (`with` Metadata False)) Cards.impromptuBarrier

instance RunMessage ImpromptuBarrier where
  runMessage msg e@(ImpromptuBarrier (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ _ zone | eid == toId attrs -> do
      sid <- getRandom
      chooseEvade <- toMessage . setTarget attrs <$> mkChooseEvade sid iid attrs
      pushAll
        $ chooseEvade
        : [ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs) | zone == FromDiscard]
      pure . ImpromptuBarrier $ attrs `with` Metadata (zone == FromDiscard)
    ChosenEvadeEnemy sid (isSource attrs -> True) eid -> do
      push $ skillTestModifier sid attrs eid (EnemyEvade (-1))
      pure e
    Successful (Action.Evade, EnemyTarget enemyId) iid _ (isTarget attrs -> True) n -> do
      enemies <-
        map fst
          . filter (maybe False (<= n) . snd)
          <$> selectWithField Enemy.EnemyEvade (at_ (locationWithEnemy enemyId) <> not_ (EnemyWithId enemyId))
      player <- getPlayer iid
      sid <- getRandom
      chooseEvade <-
        toMessage . setTarget attrs <$> mkChooseEvadeMatch sid iid attrs (oneOf $ map EnemyWithId enemies)
      pushAll
        $ EnemyEvaded iid enemyId
        : [ chooseOne
            player
            [ Label "Do not evade another enemy" []
            , Label "Evade Another Enemy" [chooseEvade]
            ]
          | notNull enemies
          ]

      pure e
    _ -> ImpromptuBarrier . (`with` meta) <$> runMessage msg attrs
