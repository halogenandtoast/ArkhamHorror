module Arkham.Story.Cards.AMomentsRest (aMomentsRest) where

import Arkham.Helpers.Investigator
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AMomentsRest = AMomentsRest StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aMomentsRest :: StoryCard AMomentsRest
aMomentsRest = story AMomentsRest Cards.aMomentsRest

instance RunMessage AMomentsRest where
  runMessage msg s@(AMomentsRest attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      enemies <- select $ EnemyOneOf [NotEnemy ExhaustedEnemy, EnemyIsEngagedWith Anyone]
      chooseOrRunOneM iid do
        scenarioI18n $ labeled' "aMomentsRest.chooseEnemy " do
          chooseTargetM iid enemies \enemy -> do
            exhaustThis enemy
            disengageFromAll enemy
        whenM (canHaveHorrorHealed attrs iid) do
          withI18n $ countVar 5 $ labeled' "healHorror" $ healHorror iid attrs 5
      ruinsOfCarcosa <- selectJust $ locationIs Locations.ruinsOfCarcosaAMomentsRest
      setAsideRuinsOfCarcosa <- getSetAsideCardsMatching $ CardWithTitle "Ruins of Carcosa"
      otherRuinsOfCarcosa <- case nonEmpty setAsideRuinsOfCarcosa of
        Nothing -> error "missing"
        Just xs -> sample xs
      push $ ReplaceLocation ruinsOfCarcosa otherRuinsOfCarcosa DefaultReplace
      pure s
    _ -> AMomentsRest <$> liftRunMessage msg attrs
