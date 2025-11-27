module Arkham.Treachery.Cards.WarpedReality (warpedReality) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WarpedReality = WarpedReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warpedReality :: TreacheryCard WarpedReality
warpedReality = treachery WarpedReality Cards.warpedReality

instance HasAbilities WarpedReality where
  getAbilities (WarpedReality a) =
    [ mkAbility a 1 $ forced $ TurnEnds #when $ You <> at_ (locationWithTreachery a)
    , restricted a 2 OnSameLocation doubleActionAbility
    ]

instance RunMessage WarpedReality where
  runMessage msg t@(WarpedReality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ basic NonWeakness <> oneOf [inHandOf NotForPlay iid, inPlayAreaOf iid]
      focusCards cards $ chooseTargetM iid cards $ hollow iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> WarpedReality <$> liftRunMessage msg attrs
