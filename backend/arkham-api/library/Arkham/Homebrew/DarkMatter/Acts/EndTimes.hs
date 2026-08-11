module Arkham.Homebrew.DarkMatter.Acts.EndTimes (endTimes) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scan, scanAction_)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Projection

newtype EndTimes = EndTimes ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endTimes :: ActCard EndTimes
endTimes = act (1, A) EndTimes Cards.endTimes Nothing

{- | "[action]: Scan. Search for the topmost card in the scanning deck with an
icon matching your current location and draw it. Shuffle the scanning deck." /
"Objective - If each undefeated investigator has resigned, advance."
-}
instance HasAbilities EndTimes where
  getAbilities (EndTimes a) =
    [ restricted a 1 (exists $ You <> at_ Anywhere) scanAction_
    , restricted a 2 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage EndTimes where
  runMessage msg a@(EndTimes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scan iid (attrs.ability 1) [symbol]
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> EndTimes <$> liftRunMessage msg attrs
