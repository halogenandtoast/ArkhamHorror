module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.TheWindowToNothingness (
  theWindowToNothingness,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musicTreacheriesInPlay, scenarioI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose

newtype TheWindowToNothingness = TheWindowToNothingness LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "X is the amount of [[Music]] treacheries in play." The printed clue value is
X, so it is seeded at zero and set when the location is revealed.
-}
theWindowToNothingness :: LocationCard TheWindowToNothingness
theWindowToNothingness = location TheWindowToNothingness Cards.theWindowToNothingness 0 (Static 0)

instance HasAbilities TheWindowToNothingness where
  getAbilities (TheWindowToNothingness a) =
    extend
      a
      [ -- "When you would leave this location: Investigate. If you fail, cancel the effects of the move."
        mkAbility a 1 $ forced $ Moves #when You AnySource (be a) Anywhere
      , {- "After doom is added to any card in play (including the agenda), each
        investigator at this location is defeated. Each enemy and asset at this
        location is discarded. Then, choose another location. Replace the chosen
        location with The Window to Nothingness, keeping the replaced location's
        connections." -}
        mkAbility a 2 $ forced $ PlacedDoomCounter #after AnySource AnyTarget
      ]

instance RunMessage TheWindowToNothingness where
  runMessage msg l@(TheWindowToNothingness attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectEach (investigatorAt attrs.id) \iid' ->
        push $ InvestigatorDefeated (attrs.ability 2) iid'
      selectEach (enemyAt attrs.id) (toDiscard (attrs.ability 2))
      selectEach (assetAt attrs.id) (toDiscard (attrs.ability 2))
      others <- select $ Anywhere <> not_ (be attrs)
      chooseOneM iid $ scenarioI18n $ scope "theWindowToNothingness" do
        targets others \lid -> push $ ReplaceLocation lid (toCard attrs) Msg.DefaultReplace
      pure l
    Msg.RevealLocation _ (is attrs -> True) -> do
      -- X is the number of Music treacheries in play at the moment it is revealed.
      x <- length <$> musicTreacheriesInPlay
      placeClues (toSource attrs) attrs x
      pure l
    _ -> TheWindowToNothingness <$> liftRunMessage msg attrs
