module Arkham.Homebrew.DarkMatter.Assets.SubElectronNoiseSensor (subElectronNoiseSensor) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Helpers.Cost (getCanAffordAdditionalActionCost)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scanAction_, scanAt)
import Arkham.Investigate (mkInvestigateLocation)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype SubElectronNoiseSensor = SubElectronNoiseSensor AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

subElectronNoiseSensor :: AssetCard SubElectronNoiseSensor
subElectronNoiseSensor = asset SubElectronNoiseSensor Cards.subElectronNoiseSensor

-- | "any revealed location with exactly 1 clue remaining"
noisyLocation :: LocationMatcher
noisyLocation = RevealedLocation <> LocationWithClues (EqualTo $ Static 1)

{- | "[action] Choose any revealed location with exactly 1 clue remaining. You
may either (choose one): Investigate. Investigate with +2 [intellect] as if you
were at that location. / Activate a Scan ability at that location."

Two abilities, not one @chooseOneM@: an ability's 'Arkham.Actions.Actions' is
AND-semantics, so one ability would take an Investigate *and* a Scan action.
-}
instance HasAbilities SubElectronNoiseSensor where
  getAbilities (SubElectronNoiseSensor a) =
    [ skillTestAbility $ withInvestigationTargets noisyLocation $ controlled_ a 1 investigateAction_
    , controlled a 2 (exists noisyLocation) scanAction_
    ]

instance RunMessage SubElectronNoiseSensor where
  runMessage msg a@(SubElectronNoiseSensor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ noisyLocation <> InvestigatableLocation
      affordable <-
        filterM
          (\lid -> getCanAffordAdditionalActionCost iid attrs (toTarget lid) #investigate)
          locations
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      chooseOneM iid $ targets affordable \lid -> do
        -- "as if you were at that location"
        abilityModifier (AbilityRef (toSource attrs) 1) (attrs.ability 1) iid (AsIfAt lid)
        investigation <- mkInvestigateLocation sid iid (attrs.ability 1) lid
        push $ CheckAdditionalActionCosts iid (toTarget lid) #investigate [toMessage investigation]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- select noisyLocation
      chooseTargetM iid locations \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scanAt iid (attrs.ability 2) lid [symbol]
      pure a
    _ -> SubElectronNoiseSensor <$> liftRunMessage msg attrs
