module Arkham.Story.Cards.TheSyndicateAllied (theSyndicateAllied) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheSyndicateAllied = TheSyndicateAllied StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSyndicateAllied :: StoryCard TheSyndicateAllied
theSyndicateAllied = persistStory $ story TheSyndicateAllied Cards.theSyndicateAllied

instance HasAbilities TheSyndicateAllied where
  getAbilities (TheSyndicateAllied a) =
    [ restricted a 1 (youExist $ InvestigatorAt $ LocationWithCardsUnderneath AnyCards)
        $ actionAbilityWithCost
        $ ClueCost (PerPlayer 1)
    , restricted
        a
        2
        ( DuringTurn You
            <> exists
              ( LocationWithCardsUnderneath AnyCards
                  <> RevealedLocation
                  <> LocationWithClues (LessThanOrEqualTo $ PerPlayer 1)
              )
        )
        $ FastAbility Free
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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        cards <- field LocationCardsUnderneath lid
        focusCards cards $ continue_ iid
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <-
        select
          $ LocationWithCardsUnderneath AnyCards
          <> RevealedLocation
          <> LocationWithClues (LessThanOrEqualTo $ PerPlayer 1)
      chooseTargetM iid locations (handleTarget iid (attrs.ability 2))
      pure s
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget lid) -> do
      cards <- field LocationCardsUnderneath lid
      chooseOrRunOneM iid $ targets cards $ drawCard iid
      pure s
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceCurrentAct attrs
      pure s
    _ -> TheSyndicateAllied <$> liftRunMessage msg attrs
