module Arkham.Story.Cards.RollingPits (RollingPits (..), rollingPits) where

import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target

newtype RollingPits = RollingPits StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rollingPits :: StoryCard RollingPits
rollingPits = story RollingPits Cards.rollingPits

instance RunMessage RollingPits where
  runMessage msg s@(RollingPits attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      seas <- select $ LocationWithTitle "Sea of Pitch"
      -- Set all of the clues from each Sea of Pitch aside.
      n <- getSum . fold <$> traverse (fieldMap LocationClues Sum) seas
      -- Discard all other tokens and attachments from each Sea of Pitch.
      for_ seas $ \sea -> do
        pushAll
          [ RemoveAllTokens (toSource attrs) (toTarget sea)
          , RemoveAllAttachments (toSource attrs) (toTarget sea)
          ]

      -- Flip this card back over and shuffle the positions of each copy of Sea
      -- of Pitch so you do not know which is which (investigators and enemies
      -- remain at their current position).

      shuffled <- shuffleM seas
      for_ (withIndex1 $ shuffled) $ \(i, l) -> do
        pushAll [SetLocationLabel l $ "seaOfPitch" <> tshow i, LocationMoved l]

      -- if more than 4 then we place 1 clue per division of 4
      let a = n `div` 4
      when (a > 0) do
        for_ seas $ \sea -> do
          placeClues attrs sea a

      -- mod 4 to get the remainder, then let investigator choose
      let b = n `mod` 4
      pushWhen (b > 0) $ DoStep b msg
      pure s
    DoStep n msg'@(ResolveStory iid ResolveIt story') | story' == toId attrs && n > 0 -> do
      seasWithClues <- selectWithField LocationClues $ LocationWithTitle "Sea of Pitch"
      let lowestCount = getMin $ foldMap (Min . snd) seasWithClues
      let seas = map fst $ filter ((== lowestCount) . snd) seasWithClues
      chooseOne
        iid
        [targetLabel sea [PlaceClues (toSource attrs) (toTarget sea) 1, DoStep (n - 1) msg'] | sea <- seas]
      pure s
    _ -> RollingPits <$> lift (runMessage msg attrs)
