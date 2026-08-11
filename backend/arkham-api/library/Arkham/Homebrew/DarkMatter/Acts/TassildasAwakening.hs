module Arkham.Homebrew.DarkMatter.Acts.TassildasAwakening (tassildasAwakening) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card.CardDef (toCardType)
import Arkham.Card.CardType
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (scan, scanAction_)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Projection

newtype TassildasAwakening = TassildasAwakening ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tassildasAwakening :: ActCard TassildasAwakening
tassildasAwakening = act (1, A) TassildasAwakening Cards.tassildasAwakening Nothing

{- | "[action]: Scan..." /
"[action] Parley. Get help from the colonies. Place 1 of your clues onto the
location with the most clues: Deal X damage to Tassilda. X is the number of story
assets in the victory display." /
"Objective - When Tassilda is defeated, advance."
-}
instance HasAbilities TassildasAwakening where
  getAbilities (TassildasAwakening a) =
    [ restricted a 1 (exists $ You <> at_ Anywhere) scanAction_
    , restricted a 2 (youExist $ InvestigatorWithClues $ atLeast 1) $ parleyAction Free
    , restricted a 3 (not_ $ exists $ enemyIs Enemies.tassilda) $ Objective $ forced AnyWindow
    ]

instance RunMessage TassildasAwakening where
  runMessage msg a@(TassildasAwakening attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scan iid (attrs.ability 1) [symbol]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectOne (LocationWithMostClues Anywhere) >>= traverse_ \lid ->
        moveTokens (attrs.ability 2) iid lid #clue 1
      stories <- count ((== AssetType) . toCardType) <$> getVictoryDisplay
      when (stories > 0)
        $ selectOne (enemyIs Enemies.tassilda)
        >>= traverse_ (nonAttackEnemyDamage (Just iid) (attrs.ability 2) stories)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TassildasAwakening <$> liftRunMessage msg attrs
