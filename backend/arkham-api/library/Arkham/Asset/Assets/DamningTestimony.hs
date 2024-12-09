module Arkham.Asset.Assets.DamningTestimony (damningTestimony, DamningTestimony (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Customization
import Arkham.Helpers.Investigator (getCanDiscoverClues, withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelfWhen)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (
  isInvestigation,
  isSkillTestInvestigator,
  isSkillTestSource,
  withSkillTest,
 )
import Arkham.Investigate
import Arkham.Investigate.Types
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Projection
import Data.Bits

newtype Metadata = Metadata {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DamningTestimony = DamningTestimony (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor DamningTestimony where
  getModifiersFor (DamningTestimony (With a _)) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      self <- modifySelfWhen a (a `hasCustomization` FabricatedEvidence) [AdditionalStartingUses 2]
      let investigatingUsingThis = liftGuardsM [isSkillTestSource a, isSkillTestInvestigator iid, isInvestigation]
      blackMail <- runMaybeT do
        guard $ a `hasCustomization` Blackmail
        investigatingUsingThis
        pure $ SkillModifier #intellect 2
      searchWarrant <- runMaybeT do
        guard $ a `hasCustomization` SearchWarrant
        investigatingUsingThis
        pure MayIgnoreLocationEffectsAndKeywords
      controller <- modified_ a iid $ maybeToList blackMail <> maybeToList searchWarrant
      pure $ self <> controller

instance HasAbilities DamningTestimony where
  getAbilities (DamningTestimony (With attrs _)) =
    [ restrictedAbility attrs 1 (ControlsThis <> exists (EnemyAt Anywhere))
        $ investigateAction (exhaust attrs)
    ]

damningTestimony :: AssetCard DamningTestimony
damningTestimony = asset (DamningTestimony . (`with` Metadata Nothing)) Cards.damningTestimony

instance RunMessage DamningTestimony where
  runMessage msg a@(DamningTestimony (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt Anywhere
      chooseOne iid $ targetLabels enemies $ only . handleTargetChoice iid attrs
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      withLocationOf iid \lid -> do
        enemyLoc <- fieldJust EnemyLocation eid
        sid <- getRandom
        investigate' <- mkInvestigate sid iid (attrs.ability 1)
        investigatable <- enemyLoc <=~> InvestigatableLocation
        if attrs `hasCustomization` Surveil && enemyLoc /= lid && investigatable
          then
            chooseOne
              iid
              [ Label
                  "Investigate the enemy's location"
                  [toMessage $ investigate' {investigateLocation = enemyLoc}]
              , Label "Investigate your location" [toMessage investigate']
              ]
          else push investigate'
      pure $ DamningTestimony $ attrs `with` Metadata (Just eid)
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      push $ DoStep 0 msg
      pure a
    DoStep n msg'@(PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      withSkillTest \sid -> do
        when (attrs.use Evidence > 0) do
          for_ (chosenEnemy meta) \eid -> do
            mAdditional <- runMaybeT do
              guard $ not $ testBit n 0
              lid <- MaybeT $ field EnemyLocation eid
              guardM $ lift $ getCanDiscoverClues IsInvestigate iid lid
              enabled <- Msg.skillTestModifier sid (attrs.ability 1) iid (DiscoveredCluesAt lid 1)
              pure
                $ Label
                  "Spend 1 Evidence to discover 1 additional clue at the chosen enemy's location."
                  [ SpendUses (attrs.ability 1) (toTarget attrs) Evidence 1
                  , enabled
                  , DoStep (setBit 0 n) msg'
                  ]

            mExtort <- runMaybeT do
              guard $ not $ testBit n 1
              guard $ attrs `hasCustomization` Extort
              guardM $ lift $ eid <=~> ReadyEnemy
              pure
                $ Label
                  "Spend 1 Evidence to automatically evade the chosen enemy."
                  [ SpendUses (attrs.ability 1) (toTarget attrs) Evidence 1
                  , EnemyEvaded iid eid
                  , DoStep (setBit 1 n) msg'
                  ]

            mExpose <- runMaybeT do
              guard $ not $ testBit n 2
              guard $ attrs `hasCustomization` Expose
              guardM $ lift $ eid <=~> NonEliteEnemy
              x <- MaybeT $ field EnemyRemainingHealth eid
              guard $ attrs.use Evidence >= x
              pure
                $ Label
                  ( "Spend X("
                      <> tshow x
                      <> ") Evidence to discard the chosen enemy if it is non-Elite. X is that enemy's remaining health."
                  )
                  [ SpendUses (attrs.ability 1) (toTarget attrs) Evidence x
                  , Msg.toDiscardBy iid (attrs.ability 1) eid
                  , DoStep (setBit 2 n) msg'
                  ]

            let choices = catMaybes [mAdditional, mExtort, mExpose]
            when (notNull choices) do
              chooseOne iid $ Label "Do not spend Evidence" [] : choices

      pure a
    _ -> DamningTestimony . (`with` meta) <$> liftRunMessage msg attrs
