module Arkham.Location.Cards.CoreOfTheVaultHeartOfTheMachine (coreOfTheVaultHeartOfTheMachine) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenarios.TheGrandVault.Helpers
import Arkham.Trait (Trait (Vault))
import Arkham.Treachery.Cards qualified as Treacheries

newtype CoreOfTheVaultHeartOfTheMachine = CoreOfTheVaultHeartOfTheMachine LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coreOfTheVaultHeartOfTheMachine :: LocationCard CoreOfTheVaultHeartOfTheMachine
coreOfTheVaultHeartOfTheMachine = location CoreOfTheVaultHeartOfTheMachine Cards.coreOfTheVaultHeartOfTheMachine 3 (Static 3)

instance HasModifiersFor CoreOfTheVaultHeartOfTheMachine where
  getModifiersFor (CoreOfTheVaultHeartOfTheMachine a) = do
    -- "Core of the Vault cannot be flooded." (applies on both sides)
    modifySelf a [CannotBeFlooded]
    -- Unrevealed (Core of the Vault) side: investigators cannot move into it.
    whenUnrevealed a $ modifySelect a Anyone [CannotEnter a.id]

instance HasAbilities CoreOfTheVaultHeartOfTheMachine where
  getAbilities (CoreOfTheVaultHeartOfTheMachine a) =
    extend a
      $ if a.revealed
        then
          [ -- [Forced] If The Inescapable is defeated at this location.
            restricted a 1 Here
              $ forced
              $ EnemyDefeated #when Anyone ByAny (enemyIs Enemies.theInescapable <> enemyAt a)
          , -- [action][action] Spend 3 [per_investigator] clues, as a group.
            restricted a 2 Here
              $ actionAbilityWithCost
              $ Costs [ActionCost 2, GroupClueCost (PerPlayer 3) (be a)]
          ]
        else
          [ -- [Forced] At the end of the round, if each Vault location has been
            -- activated, reveal Core of the Vault.
            mkAbility a 3 $ forced $ RoundEnds #when
          ]

instance RunMessage CoreOfTheVaultHeartOfTheMachine where
  runMessage msg l@(CoreOfTheVaultHeartOfTheMachine attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      -- Search all in/out-of-play areas for each Still Behind You copy and remove
      -- them from the game.
      stillBehindYous <-
        select $ IncludeOutOfPlayTreachery $ treacheryIs Treacheries.stillBehindYou
      for_ stillBehindYous removeFromGame
      -- TODO: Record `the creature was defeated`. There is no corresponding
      -- TheDrownedCityKey for this ruling yet; add the key and record it here once
      -- it exists.
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      -- TODO: Proceed to Scenario Interlude: The Vault Core. The interlude is not
      -- yet implemented; the clue-spend cost is paid via the ability cost above.
      pure l
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      activated <- getActivatedCount
      vaultCount <- selectCount $ LocationWithTrait Vault
      when (vaultCount > 0 && activated >= vaultCount) do
        push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> CoreOfTheVaultHeartOfTheMachine <$> liftRunMessage msg attrs
