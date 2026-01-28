module Arkham.Story.Cards.RollingPits (rollingPits) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RollingPits = RollingPits StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rollingPits :: StoryCard RollingPits
rollingPits = story RollingPits Cards.rollingPits

instance RunMessage RollingPits where
  runMessage msg s@(RollingPits attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      seas <- select $ LocationWithTitle "Sea of Pitch"
      -- Set all of the clues from each Sea of Pitch aside.
      n <- getSum <$> foldMapM (fieldMap LocationClues Sum) seas
      -- Discard all other tokens and attachments from each Sea of Pitch.
      for_ seas \sea -> do
        pushAll
          [ RemoveAllTokens (toSource attrs) (toTarget sea)
          , RemoveAllAttachments (toSource attrs) (toTarget sea)
          ]

      -- Flip this card back over and shuffle the positions of each copy of Sea
      -- of Pitch so you do not know which is which (investigators and enemies
      -- remain at their current position).

      shuffled <- shuffle seas
      for_ (withIndex1 shuffled) \(i, l) -> do
        pushAll [SetLocationLabel l $ "seaOfPitch" <> tshow i, LocationMoved l]

      eachInvestigator \iid -> do
        withLocationOf iid \loc -> do
          lbl <- field LocationLabel loc
          forInvestigator' iid (scenarioSpecific "rollingPits" lbl)

      selectEach (oneOf [UnengagedEnemy, MassiveEnemy] <> not_ IsSwarm) \eid -> do
        withLocationOf eid \loc -> do
          lbl <- field LocationLabel loc
          forTarget' eid (scenarioSpecific "rollingPits" lbl)

      -- if more than 4 then we place 1 clue per division of 4
      let a = n `div` 4
      when (a > 0) $ for_ seas $ placeCluesOn attrs a

      -- mod 4 to get the remainder, then let investigator choose
      let b = n `mod` 4
      pushWhen (b > 0) $ DoStep b msg
      pure s
    DoStep n msg'@(ResolveThisStory iid (is attrs -> True)) | n > 0 -> do
      seasWithClues <- selectWithField LocationClues $ LocationWithTitle "Sea of Pitch"
      let lowestCount = getMin $ foldMap (Min . snd) seasWithClues
      let seas = map fst $ filter ((== lowestCount) . snd) seasWithClues
      -- need to remove the story now so we can place clues correctly
      push $ RemoveStory (toId attrs)
      chooseOneM iid do
        questionLabeled "Distribute remaining set-aside clues among copies of Sea of Pitch."
        targets seas \sea -> do
          placeClues attrs sea 1
          doStep 1 msg'
      pure s
    ForInvestigator iid (ScenarioSpecific "rollingPits" lbl) -> do
      loc <- selectJust $ LocationWithLabel $ toResult lbl
      push $ Do (PlaceInvestigator iid $ AtLocation loc)
      pure s
    ForTarget (EnemyTarget eid) (ScenarioSpecific "rollingPits" lbl) -> do
      loc <- selectJust $ LocationWithLabel $ toResult lbl
      push $ Do (EnemyMove eid loc)
      pure s
    _ -> RollingPits <$> liftRunMessage msg attrs
