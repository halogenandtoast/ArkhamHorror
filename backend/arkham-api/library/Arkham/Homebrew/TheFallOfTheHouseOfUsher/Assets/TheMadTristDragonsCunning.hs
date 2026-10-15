module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Assets.TheMadTristDragonsCunning (
  theMadTristDragonsCunning,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (PerformAction)
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheMadTristDragonsCunning = TheMadTristDragonsCunning AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMadTristDragonsCunning :: AssetCard TheMadTristDragonsCunning
theMadTristDragonsCunning = asset TheMadTristDragonsCunning Cards.theMadTristDragonsCunning

-- | Set while the granted investigate is still owed; cleared by ability 2.
owedInvestigate :: AssetAttrs -> Bool
owedInvestigate = getMetaKey "aooFreeInvestigate"

instance HasModifiersFor TheMadTristDragonsCunning where
  getModifiersFor (TheMadTristDragonsCunning a) =
    controllerGets a
      $ [SkillModifier #intellect 1, SkillModifier #agility 1]
      <> [ActionDoesNotCauseAttacksOfOpportunity #investigate | owedInvestigate a]

instance HasAbilities TheMadTristDragonsCunning where
  getAbilities (TheMadTristDragonsCunning a) =
    [ noAOO
        $ restricted a 1 ControlsThis
        $ actionAbilityWithCost (assetUseCost a Secret 1 <> exhaust a)
    ]
      <> [ controlled a 2 NoRestriction
             $ silent
             $ oneOf [PerformAction #after You #investigate, RoundEnds #when]
         | owedInvestigate a
         ]

instance RunMessage TheMadTristDragonsCunning where
  runMessage msg a@(TheMadTristDragonsCunning attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doStep 2 msg
      pure $ overAttrs (setMetaKey "aooFreeInvestigate" True) a
    DoStep n (UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      lid <- selectJust $ locationWithInvestigator iid
      ls <- select $ connectedFrom (LocationWithId lid) <> LocationWithClues (atLeast 1)
      unless (null ls) do
        chooseOneM iid do
          targets ls \from -> do
            moveTokens (attrs.ability 1) from lid Clue 1
            doNextStep msg
          withI18n $ labeled "doNotMoveClue" nothing
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> pure $ overAttrs (unsetMetaKey "aooFreeInvestigate") a
    _ -> TheMadTristDragonsCunning <$> liftRunMessage msg attrs
