module Arkham.Act.Cards.WhereTheresSmoke (whereTheresSmoke) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getJustLocationByName, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Treacheries

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
      selectEach (InPlayEnemy AnyEnemy) $ toDiscard attrs

      placeSetAsideLocation_ Locations.dormitories_MiskatonicUniversity
      placeSetAsideLocation_ Locations.miskatonicQuad_MiskatonicUniversity

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
