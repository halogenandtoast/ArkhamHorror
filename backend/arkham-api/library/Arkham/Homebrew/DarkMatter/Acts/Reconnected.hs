module Arkham.Homebrew.DarkMatter.Acts.Reconnected (reconnected) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (scanEventFor)
import Arkham.LocationSymbol qualified as LS
import Arkham.Matcher

newtype Reconnected = Reconnected ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reconnected :: ActCard Reconnected
reconnected = act (3, A) Reconnected Cards.reconnected Nothing

{- | "After an investigator at the Cryosleep Quarters performs a scan matching
the icon below, regardless of whether it is successful or not, you may
advance."

The icon is part of the window itself — 'scanEventFor' fires @scan[Trefoil]@
alongside the general scan window — so the ability is only ever offered for a
matching scan rather than for every scan made there.
-}
instance HasAbilities Reconnected where
  getAbilities (Reconnected a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ CampaignEvent
          #after
          (Just $ InvestigatorAt $ locationIs Locations.cryosleepQuarters)
          (scanEventFor LS.Trefoil)
    ]

instance RunMessage Reconnected where
  runMessage msg a@(Reconnected attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> Reconnected <$> liftRunMessage msg attrs
