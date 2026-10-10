module Arkham.Homebrew.AgesUnwound.Locations.RitualCircle_225 (ritualCircle_225) where

import Arkham.Ability
import Arkham.Draw.Types (cardDrawAmount, cardDrawSource)
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Investigator.Types (Field (InvestigatorDrawing))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveToMatch)
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RitualCircle_225 = RitualCircle_225 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Gateway to the Past/. Set aside; Scenario III puts it into play.
ritualCircle_225 :: LocationCard RitualCircle_225
ritualCircle_225 = symbolLabel $ location RitualCircle_225 Cards.ritualCircle_225 4 (PerPlayer 2)

playerCardEffect :: SourceMatcher
playerCardEffect = SourceIsPlayerCard <> SourceIsCardEffect

getGainedResources :: [Window] -> Maybe Int
getGainedResources = \case
  [] -> Nothing
  ((windowType -> Window.GainsResources _ _ n) : _) -> Just n
  (_ : ws) -> getGainedResources ws

{- | "Haunted - Move to Front Gates. /
Forced - When an investigator at this location would draw cards, gain resources
or gain actions from a player card effect: Cancel that aspect. Then, perform the
opposite of that aspect (discard cards, lose resources, or lose actions,
respectively). /
Forced - At the end of the round: Each investigator at Ritual Circle heals 1
damage and 1 horror."

The third clause names /Ritual Circle/ where the other two say /this location/,
so it is read as every location with that title -- Scenario III has both of
them in play.

TODO(ages-unwound): the /gain actions/ third of ability 2 is unimplemented.
There is no @GainedActions@ window (only @LostActions@); @handleGainActions@
adds to @remainingActions@ with no timing point at all
(@Investigator/Runner/Action.hs:410@). This card is the first real consumer, so
the window needs adding in core.
-}
instance HasAbilities RitualCircle_225 where
  getAbilities (RitualCircle_225 a) =
    extendRevealed
      a
      [ campaignI18n $ hauntedI "ritualCircleGatewayToThePast.haunted" a 1
      , restricted a 2 Here
          $ forced
          $ oneOf
            [ GainsResources #when You playerCardEffect (atLeast 1)
            , WouldDrawCardFrom #when You (DeckOf You) playerCardEffect
            ]
      , restricted a 3 (exists $ InvestigatorAt "Ritual Circle") $ forced $ RoundEnds #when
      ]

instance RunMessage RitualCircle_225 where
  runMessage msg l@(RitualCircle_225 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveToMatch (attrs.ability 1) iid "Front Gates"
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getGainedResources -> mResources) _ -> do
      case mResources of
        -- "Cancel that aspect": the resource gain is still a queued
        -- `Do (TakeResources ...)` at the #when window
        Just n -> do
          matchingDon't \case
            Do (TakeResources iid' _ _ False) -> iid' == iid
            _ -> False
          loseResources iid (attrs.ability 2) n
        Nothing -> do
          mDrawing <- field InvestigatorDrawing iid
          for_ mDrawing \drawing -> do
            -- the window gated on this, but a sibling reaction in the same
            -- window can still ReplaceCurrentCardDraw out from under us
            whenM (sourceMatches (cardDrawSource drawing) playerCardEffect) do
              push $ Instead (DoDrawCards iid) (Run [])
              chooseAndDiscardCards iid (attrs.ability 2) (cardDrawAmount drawing)
      pure l
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      selectEach (InvestigatorAt "Ritual Circle") \iid -> do
        healDamage iid (attrs.ability 3) 1
        healHorror iid (attrs.ability 3) 1
      pure l
    _ -> RitualCircle_225 <$> liftRunMessage msg attrs
