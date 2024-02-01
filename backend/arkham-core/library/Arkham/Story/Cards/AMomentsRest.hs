module Arkham.Story.Cards.AMomentsRest (
  AMomentsRest (..),
  aMomentsRest,
) where

import Arkham.Prelude

import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype AMomentsRest = AMomentsRest StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

aMomentsRest :: StoryCard AMomentsRest
aMomentsRest = story AMomentsRest Cards.aMomentsRest

instance RunMessage AMomentsRest where
  runMessage msg s@(AMomentsRest attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      enemies <-
        selectList
          $ EnemyOneOf [NotEnemy ExhaustedEnemy, EnemyIsEngagedWith Anyone]
      ruinsOfCarcosa <- selectJust $ locationIs Locations.ruinsOfCarcosaAMomentsRest
      mHealHorror <- getHealHorrorMessage (toSource attrs) 5 iid
      player <- getPlayer iid
      let
        handleEnemy enemy =
          targetLabel
            enemy
            [Exhaust (EnemyTarget enemy), DisengageEnemyFromAll enemy]
        choices =
          [ Label
            "Choose an enemy in play. Exhaust that enemy and disengage it from all investigators"
            [chooseOne player $ map handleEnemy enemies]
          | notNull enemies
          ]
            <> [ Label "You heal 5 horror" [healHorror]
               | healHorror <- maybeToList mHealHorror
               ]
      setAsideRuinsOfCarcosa <-
        getSetAsideCardsMatching
          $ CardWithTitle "Ruins of Carcosa"
      otherRuinsOfCarcosa <- case nonEmpty setAsideRuinsOfCarcosa of
        Nothing -> error "missing"
        Just xs -> sample xs
      pushAll
        $ [chooseOrRunOne player choices | notNull choices]
        <> [ReplaceLocation ruinsOfCarcosa otherRuinsOfCarcosa DefaultReplace]
      pure s
    _ -> AMomentsRest <$> runMessage msg attrs
