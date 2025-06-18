module Arkham.Story.Cards.TheSyndicateAllied (theSyndicateAllied) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheSyndicateAllied = TheSyndicateAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSyndicateAllied :: StoryCard TheSyndicateAllied
theSyndicateAllied = persistStory $ story TheSyndicateAllied Cards.theSyndicateAllied

instance HasAbilities TheSyndicateAllied where
  getAbilities (TheSyndicateAllied a) =
    [ restricted a 1 Here $ actionAbilityWithCost $ ClueCost (PerPlayer 1)
    , restricted a 2 (DuringTurn You) $ FastAbility Free
    , restricted
        a
        3
        ( exists
            $ assetIs Assets.jewelOfSarnath
            <> AssetControlledBy (InvestigatorAt $ locationIs Locations.lobbyTheMidwinterGala)
        )
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 4) Anywhere)
    ]

instance RunMessage TheSyndicateAllied where
  runMessage msg s@(TheSyndicateAllied attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure s
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceCurrentAct attrs
      pure s
    _ -> TheSyndicateAllied <$> liftRunMessage msg attrs
