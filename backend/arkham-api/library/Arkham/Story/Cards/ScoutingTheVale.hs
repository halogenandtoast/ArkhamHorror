module Arkham.Story.Cards.ScoutingTheVale (ScoutingTheVale (..), scoutingTheVale) where

import Arkham.Card
import Arkham.Game.Helpers (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy
import Arkham.Target

newtype ScoutingTheVale = ScoutingTheVale StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scoutingTheVale :: StoryCard ScoutingTheVale
scoutingTheVale = story ScoutingTheVale Cards.scoutingTheVale

instance RunMessage ScoutingTheVale where
  runMessage msg s@(ScoutingTheVale attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      n <- perPlayer 2
      lookAt iid attrs EncounterDeckTarget [fromTopOfDeck n] #any (defer attrs IsNotDraw)
      cragOfTheGhouls <- selectJust $ locationIs Locations.cragOfTheGhouls
      gameModifier attrs cragOfTheGhouls Blank
      pure s
    SearchFound _iid (isTarget attrs -> True) _ cards | notNull cards -> do
      let x = length cards `div` 2
      push $ DoStep x msg
      pure s
    DoStep x msg'@(SearchFound iid (isTarget attrs -> True) sig cards) | x > 0 -> do
      chooseOne iid
        $ [Label "Done discarding" [msg']]
        <> [ TargetLabel
            (CardIdTarget $ toCardId c)
            [AddToEncounterDiscard ec, DoStep (x - 1) (SearchFound iid (toTarget attrs) sig rest)]
           | (c@(EncounterCard ec), rest) <- eachWithRest cards
           ]
      pure s
    DoStep 0 (SearchFound iid (isTarget attrs -> True) sig cards) | notNull cards -> do
      chooseOne
        iid
        [ TargetLabel
          (CardIdTarget $ toCardId c)
          [PutCardOnTopOfDeck iid sig c, DoStep 0 (SearchFound iid (toTarget attrs) sig rest)]
        | (c, rest) <- eachWithRest cards
        ]
      pure s
    _ -> ScoutingTheVale <$> liftRunMessage msg attrs
