module Arkham.Story.Cards.StarsOfAldebaran (
  StarsOfAldebaran (..),
  starsOfAldebaran,
) where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype StarsOfAldebaran = StarsOfAldebaran StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starsOfAldebaran :: StoryCard StarsOfAldebaran
starsOfAldebaran = story StarsOfAldebaran Cards.starsOfAldebaran

instance RunMessage StarsOfAldebaran where
  runMessage msg s@(StarsOfAldebaran attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      healableInvestigators <- select $ HealableInvestigator (toSource attrs) #horror Anyone
      let healMessages = [HealHorror (toTarget iid') (toSource attrs) 3 | iid' <- healableInvestigators]
      enemies <- select $ NotEnemy $ EnemyWithTitle "Hastur"
      bleakPlains <- selectJust $ locationIs Locations.bleakPlainsStarsOfAldebaran
      let
        damageEnemy enemy =
          targetLabel
            enemy
            [Msg.EnemyDamage enemy $ storyDamage iid 4]
      setAsideBleakPlains <- getSetAsideCardsMatching $ CardWithTitle "Bleak Plains"
      otherBleakPlain <- case setAsideBleakPlains of
        [] -> error "missing"
        (x : xs) -> sample (x :| xs)
      player <- getPlayer iid
      pushAll
        $ healMessages
        <> [chooseOrRunOne player $ map damageEnemy enemies | notNull enemies]
        <> [ReplaceLocation bleakPlains otherBleakPlain DefaultReplace]
      pure s
    _ -> StarsOfAldebaran <$> runMessage msg attrs
