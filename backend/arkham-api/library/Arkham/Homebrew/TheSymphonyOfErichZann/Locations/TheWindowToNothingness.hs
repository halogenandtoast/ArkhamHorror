module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.TheWindowToNothingness (
  theWindowToNothingness,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Location (getLocationGlobalMeta)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musicTreacheriesInPlay, scenarioI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose

newtype TheWindowToNothingness = TheWindowToNothingness LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Joining the Window to the map.

It prints no symbol, and every connection in this scenario is drawn between
symbols, so nothing would ever be adjacent to it. Beyond the Curtain "attaches"
it to the Auditorium, which is where it starts; its own Forced then moves it by
replacing a location "keeping the replaced location's connections", and
`ReplaceLocation` records which card that was, so its printed connections can be
read back off the def. Either way the link is granted in both directions.
-}
instance HasModifiersFor TheWindowToNothingness where
  getModifiersFor (TheWindowToNothingness a) = do
    mReplaced <- getLocationGlobalMeta @CardCode "replacedLocation" a
    let
      neighbors = case mReplaced >>= lookupCardDef >>= nonEmptyConnections of
        Just symbols -> oneOf [LocationWithSymbol s | s <- symbols]
        Nothing -> locationIs Cards.auditorium
    modifySelf a [ConnectedToWhen Anywhere neighbors]
    modifySelect a neighbors [ConnectedToWhen Anywhere (be a)]
   where
    nonEmptyConnections def = case cdLocationRevealedConnections def of
      [] -> Nothing
      symbols -> Just symbols

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
      selectEach (investigatorAt attrs.id) $ push . InvestigatorDefeated (attrs.ability 2)
      selectEach (enemyAt attrs.id) (toDiscard (attrs.ability 2))
      selectEach (assetAt attrs.id) (toDiscard (attrs.ability 2))
      others <- select $ Anywhere <> not_ (be attrs)
      chooseOneM iid $ scenarioI18n $ scope "theWindowToNothingness" do
        targets others \lid -> do
          -- `Swap` is what carries the replaced location's cell and label over;
          -- `DefaultReplace` would drop the Window off the map again. It would
          -- also take that location's revealed state, and the Window has no
          -- unrevealed face.
          setGlobal lid "replacedIsRevealed" True
          push $ ReplaceLocation lid (toCard attrs) Msg.Swap
      pure l
    Msg.RevealLocation _ (is attrs -> True) -> do
      -- X is the number of Music treacheries in play at the moment it is revealed.
      x <- length <$> musicTreacheriesInPlay
      placeClues (toSource attrs) attrs x
      pure l
    _ -> TheWindowToNothingness <$> liftRunMessage msg attrs
