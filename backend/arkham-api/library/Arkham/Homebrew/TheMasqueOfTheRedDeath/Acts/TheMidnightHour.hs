module Arkham.Homebrew.TheMasqueOfTheRedDeath.Acts.TheMidnightHour (theMidnightHour) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype TheMidnightHour = TheMidnightHour ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMidnightHour :: ActCard TheMidnightHour
theMidnightHour = act (2, A) TheMidnightHour Cards.theMidnightHour Nothing

-- | Act 1's advance flipped it back over, so act 2 has to buy the exit open again.
unrevealedGrandBallroom :: LocationMatcher
unrevealedGrandBallroom = locationIs Locations.grandBallroom <> UnrevealedLocation

instance HasAbilities TheMidnightHour where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists unrevealedGrandBallroom)
        $ FastAbility (GroupClueCost (PerPlayer 4) Anywhere)
    , onlyOnce $ restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage TheMidnightHour where
  runMessage msg a@(TheMidnightHour attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach unrevealedGrandBallroom reveal
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    -- Both printed branches of 2b point to (->R2); only their flavor differs, and
    -- the players read that off the flipped card.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> TheMidnightHour <$> liftRunMessage msg attrs
