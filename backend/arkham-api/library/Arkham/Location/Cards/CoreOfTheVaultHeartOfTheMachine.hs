module Arkham.Location.Cards.CoreOfTheVaultHeartOfTheMachine (coreOfTheVaultHeartOfTheMachine) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Key (
  TheDrownedCityKey (TheCreatureWasDefeated, TheInnerSanctumWasUnsealed),
 )
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
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
              $ doubleActionAbilityWithCost
              $ GroupClueCost (PerPlayer 3) (be a)
          ]
        else
          [ restricted a 3 (not_ $ exists $ LocationWithTrait Vault <> LocationWithResources (atMost 0))
              $ forced
              $ RoundEnds #when
          ]

instance RunMessage CoreOfTheVaultHeartOfTheMachine where
  runMessage msg l@(CoreOfTheVaultHeartOfTheMachine attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      -- Search the encounter deck, discard pile, and all in and out of play areas
      -- for each copy of Still Behind You and remove them from the game. Selecting
      -- treacheries only finds the ones that exist as entities, so the copies still
      -- sitting in the encounter deck survived; this message reaches the scenario's
      -- deck/discard/set-aside as well as the treachery entities.
      push $ Msg.RemoveAllCopiesOfEncounterCardFromGame (cardIs Treacheries.stillBehindYou)
      record TheCreatureWasDefeated
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> scenarioI18n $ scope "theVaultCore" do
      -- "Proceed to Scenario Interlude: The Vault Core." The clue spend is paid as
      -- the ability's cost above; play continues either way, so neither branch ends
      -- the scenario.
      storyWithChooseOneM'
        do
          h "title"
          p "body"
          p "node"
          p.basic "choose"
          ul do
            li "pushTheButton"
            li "leaveItAlone"
        do
          labeled' "pushTheButton" $ record TheInnerSanctumWasUnsealed
          labeled' "leaveItAlone" $ interludeXpAll $ toBonus "leaveItAlone" 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      activated <- getActivatedCount
      vaultCount <- selectCount $ LocationWithTrait Vault
      when (vaultCount > 0 && activated >= vaultCount) do
        push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> CoreOfTheVaultHeartOfTheMachine <$> liftRunMessage msg attrs
