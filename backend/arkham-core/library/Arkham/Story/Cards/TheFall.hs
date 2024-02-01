module Arkham.Story.Cards.TheFall (
  TheFall (..),
  theFall,
) where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheFall = TheFall StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theFall :: StoryCard TheFall
theFall = story TheFall Cards.theFall

instance RunMessage TheFall where
  runMessage msg s@(TheFall attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 2
      lid <- selectJust $ locationIs Locations.darkSpires
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
              "You cannot bring yourself to do what must be done."
              [SetFlippable lid True]
          , Label
              "Realizing what you must do, you step forward and push her."
              [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
              , Msg.EnemyDamage hastur $ storyDamage iid n
              ]
          ]
      pure s
    _ -> TheFall <$> runMessage msg attrs
