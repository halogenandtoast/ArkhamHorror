module Arkham.Act.Cards.BrethrenOfAsh.SpreadingFlames.WhereTheresSmoke (whereTheresSmoke) where

import Arkham.Ability
import Arkham.Act.CardDefs.BrethrenOfAsh.SpreadingFlames qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.BrethrenOfAsh.SpreadingFlames qualified as Enemies
import Arkham.Helpers.Query (getJustLocationByName, getSetAsideCardsMatching)
import Arkham.Location.CardDefs.BrethrenOfAsh.MiskatonicUniversity qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.CardDefs.BrethrenOfAsh.Fire1 qualified as Treacheries

newtype WhereTheresSmoke = WhereTheresSmoke ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereTheresSmoke :: ActCard WhereTheresSmoke
whereTheresSmoke = act (1, A) WhereTheresSmoke Cards.whereTheresSmoke Nothing

instance HasAbilities WhereTheresSmoke where
  getAbilities (WhereTheresSmoke x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

instance RunMessage WhereTheresSmoke where
  runMessage msg a@(WhereTheresSmoke attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (AnyEnemy) $ toDiscard attrs

      placeSetAsideLocation_ Locations.dormitories
      placeSetAsideLocation_ Locations.miskatonicQuad

      createSetAsideEnemy_ Enemies.servantOfFlameRagingFury (location_ "Dormitories")

      getSetAsideCardsMatching (cardIs Treacheries.fire1) >>= \case
        (fire : rest) -> do
          yourFriendsRoom <- getJustLocationByName "Your Friend's Room"
          obtainCard fire
          createTreacheryAt_ fire (AttachedToLocation yourFriendsRoom)
          addToEncounterDiscard rest
        [] -> pure ()

      advanceActDeck attrs
      pure a
    _ -> WhereTheresSmoke <$> liftRunMessage msg attrs
