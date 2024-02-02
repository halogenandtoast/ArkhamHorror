module Arkham.Treachery.Cards.Pitfall (
  pitfall,
  Pitfall (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Deck
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Pitfall = Pitfall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

pitfall :: TreacheryCard Pitfall
pitfall = treachery Pitfall Cards.pitfall

instance RunMessage Pitfall where
  runMessage msg t@(Pitfall attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      card <- field TreacheryCard (toId attrs)
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Test {agility} (3) to attempt to jump the gap. For each point you fail by, take 1 damage."
          [RevelationSkillTest iid source SkillAgility 3]
        : [ Label
            "Shuffle Pitfall into the exploration deck. You cannot choose this option if Pitfall was drawn from the exploration deck."
            [ ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) [card]
            , RemoveTreachery (toId attrs)
            ]
          | treacheryDrawnFrom attrs /= Just (ScenarioDeckByKey ExplorationDeck)
          ]
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny n 0
      pure t
    _ -> Pitfall <$> runMessage msg attrs
