module Arkham.Homebrew.DarkMatter.Acts.TassildasAwakening (tassildasAwakening) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card (cardMatch)
import Arkham.Helpers.Game (getRemovedFromPlayCards)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (scanAction_, scanAtYourLocation)
import Arkham.Matcher

newtype TassildasAwakening = TassildasAwakening ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Set aside at setup; Ritual of the Sun replaces the act deck with it.
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
    , restricted
        a
        2
        (youExist (InvestigatorWithClues $ atLeast 1) <> exists (LocationWithMostClues Anywhere))
        $ parleyAction Free
    , mkAbility a 3
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny (enemyIs Enemies.tassilda)
    ]

instance RunMessage TassildasAwakening where
  runMessage msg a@(TassildasAwakening attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanAtYourLocation iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectOne (LocationWithMostClues Anywhere) >>= traverse_ \lid ->
        moveTokens (attrs.ability 2) iid lid #clue 1
      stories <- count (`cardMatch` CardIsStoryAsset) <$> getVictoryDisplay
      when (stories > 0)
        $ selectOne (enemyIs Enemies.tassilda)
        >>= traverse_ (nonAttackEnemyDamage (Just iid) (attrs.ability 2) stories)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs attrs
      pure a
    {- Act 1b:

    "If Hope, Yuggoth and New Brooklyn have been removed from the game: Advance
    to agenda 1d. (Proceed to the back of the Dark Matter agenda.)
    Otherwise: -> Resolution 2."

    Only the Dark Matter agenda removes a location from the game, and it records
    the card as it does so. -}
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      removed <- getRemovedFromPlayCards
      let wasRemoved def = any (`cardMatch` cardIs def) removed
      if all wasRemoved [Locations.hope, Locations.yuggoth, Locations.newBrooklyn]
        then advanceCurrentAgenda attrs
        else push R2
      pure a
    _ -> TassildasAwakening <$> liftRunMessage msg attrs
