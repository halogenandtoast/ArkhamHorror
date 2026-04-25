module Arkham.Act.Cards.WhereTheresSmoke_c2026 (whereTheresSmoke_c2026) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (enemiesAt, getJustLocationByName, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Treacheries

newtype WhereTheresSmoke_c2026 = WhereTheresSmoke_c2026 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereTheresSmoke_c2026 :: ActCard WhereTheresSmoke_c2026
whereTheresSmoke_c2026 = act (1, A) WhereTheresSmoke_c2026 Cards.whereTheresSmoke_c2026 (groupClueCost $ PerPlayer 2)

instance HasAbilities WhereTheresSmoke_c2026 where
  getAbilities (WhereTheresSmoke_c2026 x) =
    [ mkAbility x 1
        $ Objective
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

instance RunMessage WhereTheresSmoke_c2026 where
  runMessage msg a@(WhereTheresSmoke_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      yfroom <- getJustLocationByName "Your Friend's Room"
      enemies <- enemiesAt yfroom

      for_ enemies (toDiscard attrs)

      placeSetAsideLocation_ Locations.dormitories_c2026
      placeSetAsideLocation_ Locations.miskatonicQuad_c2026

      createSetAsideEnemy_
        Enemies.servantOfFlameRagingFury
        (location_ "Dormitories")

      fireCards <- getSetAsideCardsMatching $ cardIs Treacheries.fire1
      case fireCards of
        (fire : rest) -> do
          obtainCard fire
          createTreacheryAt_ fire (AttachedToLocation yfroom)
          addToEncounterDiscard rest
        [] -> pure ()

      advanceActDeck attrs
      pure a
    _ -> WhereTheresSmoke_c2026 <$> liftRunMessage msg attrs
