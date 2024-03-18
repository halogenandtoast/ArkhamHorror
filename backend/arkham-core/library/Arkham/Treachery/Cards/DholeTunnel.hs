module Arkham.Treachery.Cards.DholeTunnel (dholeTunnel, DholeTunnel (..)) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Window (checkWindows)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers qualified as Msg
import Arkham.Treachery.Import.Lifted
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype DholeTunnel = DholeTunnel TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dholeTunnel :: TreacheryCard DholeTunnel
dholeTunnel = treachery DholeTunnel Cards.dholeTunnel

instance RunMessage DholeTunnel where
  runMessage msg t@(DholeTunnel attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mSlitheringDhole <- selectOne $ enemyIs Enemies.slitheringDhole
      case mSlitheringDhole of
        Just slitheringDhole -> do
          unengaged <- selectNone $ investigatorEngagedWith slitheringDhole
          movedWindow <- checkWindows [mkWhen (Window.MovedFromHunter slitheringDhole)]

          pushAll
            $ [ Ready (toTarget slitheringDhole)
              ]
            <> ( guard unengaged
                  *> [movedWindow, HunterMove slitheringDhole]
               )
        Nothing -> do
          nearestDholeTunnel <-
            select $ NearestLocationTo iid (LocationWithTreachery $ treacheryIs Cards.dholeTunnel)
          if null nearestDholeTunnel
            then withLocationOf iid $ attachTreachery attrs
            else
              chooseOrRunOne
                iid
                [ targetLabel location [HandleTargetChoice iid (toSource attrs) (toTarget location)]
                | location <- nearestDholeTunnel
                ]
      -- attach Dhole Tunnel to a location at least 2 connections away from the nearest Dhole Tunnel (or to your location if you cannot)
      -- if Slithering Dhole is in the victory display, spawn it at attached location, exhausted.
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      targets <- select $ LocationWithDistanceFromAtLeast 2 lid Anywhere
      chooseOrRunOne
        iid
        [targetLabel location [Msg.attachTreachery (toId attrs) (toTarget location)] | location <- targets]
      pure t
    _ -> DholeTunnel <$> lift (runMessage msg attrs)
